(*w
 * ====Open trees====
 *
 * This is a camlp4 extension used to define extensible reccursive types.
 * Extension is done both through (hidden) open reccursion and polymorphic
 * variant types.
 *
 * In order to ease tree transformations and traversal a module performing a
 * monadic Map also generated.
 *
 * All the intermediates trees of the compiler are built via this extension. A
 * good number of the passes in compiler also use the defined mapping modules.
 *
 * //WON'T FIX://
 * Error reporting is currently extremelly bad since no efforts have been
 * made. This extension is used only internally (AFAIK) so it seems to much of
 * a hassle for the benefit.
 *
 * //TODO://
 *
 * _Multiple inheritence (import .... from? oop?)
 * _When extending a grammar [G] that doesn't have type [t] we still currently
 *  need the type [t] in [G] if we are to use it in the extension. This seems
 *  like a design mistake. We should know the whole list of types so we can
 *  generate a function that dynamically fails or goes to an abstract type.
 *
 *)

open General
open Camlp4.PreCast
open Syntax
open Ast
open Camlp4Helper
open Grammar.Types

(*w
 * Syntax extension CLI options...
 *)
let noMap = ref false

let () = Camlp4.Options.add "-no_map" (Arg.Set noMap) "Do not generate mapper module"


let genTrav gram=
 <:str_item<
  (*The mapper*)
    module type T =
    sig
     $Grammar.fold gram
      ~init:<:sig_item< >>
      ~f:fun ~key:i ~data:_ acc ->
       <:sig_item< type $lid:i$ ;; $acc$ >>
      $
    end;;

    module Conv(From:T)(To:T)(Mon:Monad.T)=
    struct
     (*Monadic functions...**)
     $match Grammar.super gram with
      | None -> <:str_item< >>
      | Some i ->
          let conv = <:ident< $id:i$.Trav.Conv >> in
          <:str_item<module Super = $id:conv$(From)(To)(Mon) >>
          $;;

     (*w
      * Represents a full transformation of the tree from the types defined in
      * ^^From^^ to the types defined in ^^To^^.
      *)
      module type Translation =
      sig
       $Grammar.fold gram
        ~init:<:sig_item< >>
        ~f:(fun ~key:i ~data:_ acc ->
               <:sig_item<
              val $i$ : From.$lid:i$ -> To.$lid:i$ Mon.m;;
              $acc$
              >>)$
      end

     (*w
      * These are the object types used to close the loop in ^^From^^ and
      * ^^To^^. They are used to define partial transformation's type.
      *)
      type from_dict = $OpenType.loopbackType gram <:ident<From>>$
      type to_dict = $OpenType.loopbackType gram <:ident<To>>$

      (*w
       * The whole tree is translated From -> To excepted the root which is kept
       * identical. In case of a Map partial transformations and full
       * transformations are identical.
       *)
      module type PartialTranslation =
      sig
        $Grammar.fold gram
        ~init:<:sig_item< >>
        ~f:(fun ~key:i ~data:_ acc -> <:sig_item<
            val $i$ : from_dict Gram.$lid:i$ -> to_dict Gram.$lid:i$ Mon.m;;
            $acc$>>)$
      end;;

      module In =
      struct
        $Grammar.fold gram
          ~init:<:str_item< >>
          ~f:(fun ~key:i ~data:_ acc -> <:str_item<
              type $lid:i$ = from_dict Gram.$lid:i$;;
              $acc$>>)$
      end;;

      (*w
       * This is used to close the reccursion.
       *)
      module CloseRec(E:functor(T:Translation) -> Translation) : Translation =
      struct
       module rec T:Translation=E(T)
       include T
      end

      (*Performs a monadic traversal of the tree...*)
      module Base (Self:Translation) : PartialTranslation =
      struct
       module MonHelp=Monad.Helper(Mon);;
       $let st = match Grammar.super gram with
        | None -> <:str_item< >>
        | Some _ -> <:str_item<include Super.Base(Self) >>
      in
      <:str_item< $st$;; $Mappers.gen gram$>>
       $
    end
   end

(*w
 * The following two modules are used to define automatically the boilerplate
 * code used for common cases.
 *)
   module TranslateFrom(From:T)(Mon:Monad.T)=Conv(From)(ClosedDef)(Mon)

   module Map(Mon:Monad.T)=Conv(ClosedDef)(ClosedDef)(Mon)
 >>

EXTEND Gram
 GLOBAL: str_item;

 str_item: LEVEL "top" [[
  "grammar"; name = OPT a_UIDENT; e = OPT extends ; "=" ; "begin" ;
     it=gram_items ; "end" ->
   let gram=
    Grammar.init e it
   in
   let res=
   <:str_item<
    (*The open grammar*)
    module Gram = struct
     $OpenType.gen gram$
    end;;
     $if !noMap then
      OpenType.close gram <:ident<Gram>>
    else
     <:str_item<
    module ClosedDef=struct
     $OpenType.close gram <:ident<Gram>>$
    end
    include ClosedDef
    (*The mapper*)
    module Trav =
    struct
     $genTrav gram$
    end
     >>$
 >>
 in
 match name with
  | None -> res
  | Some n ->
   <:str_item<
   module $n$ = struct
    $res$
   end;;
   >>
]];

 (* TODO: the last opt doesn't work*)
 gram_items:[[
    it = LIST0 [ gram_item ] SEP ";"  -> List.flatten it
 ]];

 (* TODO fold the import with the gram definition

    gram ... extends ... (...,...,...) {
    }
 *)
 gram_item:[[
    id=a_LIDENT ; ":=" ; OPT "|" ; l = LIST1 variant SEP "|" ->
     [id,Variant l]
  | id=a_LIDENT ; ":=" ; OPT "|" ; l = LIST1 rule_branch SEP "|" ->
     if List.for_all l ~f:(fun x -> x=Super) then
      [id,Import]
     else
      [id,PolVar l]
  | id=a_LIDENT ; ":=" ; r = rule_item ->
     [id,Alias r]
  | id=a_LIDENT ->
     [id,Abstract]
 ]];

 rule_item:[[
  id = a_LIDENT ->
    Atom id
  | "[" ; i = rule_item ; "]" ->
     List i
  | "[" ; l = LIST1 rule_item SEP "," ; "]" ->
     List (Tup l)
  | "("; l = LIST1 rule_item SEP "," ;")" ->
     Tup l
  |  i = rule_item ; "?" ->
     Option i
  |  ty = ctyp -> Nativ ty
 ]];

 variant_args: [[
  "("; l = LIST1 rule_item SEP ","; ")" -> l
  | r = rule_item -> [r]
  | -> []
 ]];

 variant: [[
   id = a_UIDENT; l = variant_args -> (id,l)
 ]];

 rule_branch:[[
    "`";id = a_UIDENT; l = variant_args -> Labeled (id,l)
  | "super" -> Super
  | id = a_LIDENT -> Other id
 ]];

 uid:[[
  i = LIST1 a_UIDENT SEP "." ->
    let rec uid2ident = function
     | [a]-> <:ident<$uid:a$>>
     | a::b -> <:ident<$uid:a$.$uid2ident b$>>
     | [] -> assert false
    in
    uid2ident i
 ]];

 (*grammar inheritence...*)
 extends:[[
  "extends"; i=uid ; "("; imports = LIST1 a_LIDENT SEP ";" ; ")" ->
    <:ident< $i$ >>,imports
 ]];

END
