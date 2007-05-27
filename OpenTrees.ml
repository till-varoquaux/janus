(*w
  ====Open trees====

  This is a camlp4 extension used to define extensible reccursive
  types. Extension is done both through (hidden) open reccursion and polymorphic
  variant types.

  In order to ease tree transformations and traversal a module performing a
  monadic Map also generated.

  All the intermediates trees of the compiler are built via this extension. A
  good number of the passes in compiler also use the defined mapping modules.

  //WON'T FIX://
  Error reporting is currently extremelly bad since no efforts have been
  made. This extension is used only internally (AFAIK) so it seems to much of
  a hassle for the benefit.
*)

(*w
===Header===
  We are mostly doing some bookeeping here, mainly administrative stuff: imports
  and simple helper functions.
==General==
*)
module StringMap=Map.Make(String)

(*w
  Ocaml's stdlib seems to be strangely lacking of some basic features. This
  module stub is inspired by
  [[http://ocaml-lib.sourceforge.net/doc/Option.html|ExtLib's option
  module]].
*)
module Option=
 struct
  exception No_value

  let get = function
   | Some v -> v
   | None -> raise No_value

  let map f= function
   | Some x -> Some (f x)
   | None -> None
 end

(*w
  ==Camlp4==
  A bunch of generic functions usefull to create nodes in camlp4
*)
open Camlp4.PreCast
open Syntax
open Ast

let _loc=Loc.ghost
(*w
   Generates a fresh id..
 *)
let fresh=
 let cpt=ref 0 in
 fun () ->
  incr cpt;
  <:ident< $lid:Printf.sprintf "id_%i" !cpt$ >>

(*w
   Transforms an expression list to a tupple of the given expressions.
*)
let exList2ExCom= function
 | [] -> <:expr< >>
 | [e] -> e
 | l ->
    let r=List.fold_left begin fun tup t ->
     Ast.ExCom (_loc,tup,t)
    end <:expr< >> l in
    ExTup(_loc,r)

(*w
  Transforms a  pattern list to a tupple of the given patterns.
*)
let pattList2Pattern = function
 | [] -> <:patt< >>
 | [i] -> i
 | l ->
    let r =List.fold_left begin fun tup i ->
     Ast.PaCom (_loc,tup,i)
    end <:patt< >> l in
    Ast.PaTup(_loc,r)

(*w
  Converts a list of match cases to the matchcase corresponding to the sum of
  all the given match cases.
*)
let matchcaseList2matchcase l =
 List.fold_left begin fun acc s ->
  <:match_case< $acc$ | $s$ >>
 end <:match_case< >> l


(*w
  ==Grammar definition==

*)
(*w This is our basic type definition for grammars. ^^super^^ designs the parent
grammar when making an extension.
*)
type grammar=
  {
   rules:ruleRHS StringMap.t;
   super:Ast.ident option
  }
and ruleRHS=
 | Abstract
 | Import
 | Variant of branch list
 | Alias of rule_item
and branch =
 | Other of string
 | Labeled of string * rule_item list
 | Super
and rule_item =
 | Option of rule_item
 | Atom of string
 | Tup of rule_item list
 | List of rule_item

(*w
  This module is just a set of rules to ease the handling of grammars, and make
  them more abstract.
*)
module ExtGram=
 struct

  let mem key g =
   StringMap.mem key g.rules

  let fold f g a =
   StringMap.fold f g.rules a

  let init super gramItems=
   let rules=
    List.fold_left (fun m (k,v) -> StringMap.add k v m) StringMap.empty gramItems
   in
   {rules=rules;
    super=super
   }

 end

module G=ExtGram


(*w ==Generate the open type==
  [@OpenType@]
  Grammar types are defined under the hood using open reccursion and
  constraints. They will later on be closed via the type parameter (which is an
  object type).

  [[#OpenType.gen|gen]] will translate grammars from:
  %%
  tree:=`Branch tree,tree | `Leaf elem
  elem:=int
  %%
  to:
  %%
  type 'cst tree= [`Branch of 'tree,'tree | `Leaf of 'elem]
  constraint 'cst=<tree:'tree;elem:'elem;..>
  type 'cst elem=
  constraint 'cst=<tree:'tree;elem:'elem;..>
  %%
  The parameter ^^'cst^^ will later on be used to close the loop.


  [[#OpenType.gen|close]] can then be used to close the open reccursion an get
  rid of the type parameter. Suppose the previous grammar was in a module called
  ^^Gram^^ we would obtain the corresponding closed types:
  %%
  type cst=<tree:tree;elem:elem>
  and tree=cst Gram.tree
  and elem=cst Gram.elem
  %%
*)

module OpenType:
 sig
  val gen:grammar -> Ast.str_item
  val close:grammar -> Ast.ident -> Ast.str_item
  val loopbackType: grammar -> Ast.ctyp
 end
 =
 struct
  (*w
     Generates the type for a ruleItem
  *)
  let rec ruleItem gram = function
   | Option r -> <:ctyp< $ruleItem gram r$ option >>
   | Tup l -> ruleItems gram l
   | Atom s when G.mem s gram -> <:ctyp< '$lid:s$ >>
   | Atom s -> <:ctyp< $lid:s$ >>
   | List l -> <:ctyp< $ruleItem gram l$ list >>

  and ruleItems gram=
   let convert=ruleItem gram in
   function
    | [] -> <:ctyp< >>
    | [r] -> convert r
    | l ->
       let t=List.fold_left begin fun tup t ->
        Ast.TySta (_loc,tup,convert t)
       end <:ctyp< >> l in
       <:ctyp< $tup:t$ >>;;

  let ruleBranch gram super = function
   | Other s -> <:ctyp< $lid:s$ >>
   | Labeled (s,rl) -> <:ctyp< `$s$ of $ruleItems gram rl$ >>
   | Super ->  Option.get super

  let ruleRHS gram super=function
   | Abstract -> <:ctyp< >>
   | Import -> Option.get super
   | Alias i -> ruleItem gram i
   | Variant rb ->
      let body=
       List.fold_left begin fun acc s ->
        <:ctyp< $acc$ | $ruleBranch gram super s$ >>
       end <:ctyp< >> rb in
      <:ctyp< [ $body$ ] >>;;

  (*
    Generates the open type def corresponding to one rule in the grammar.
    constr is the constraint on the tye parameter.
  *)
  let rule constr gram name (l:ruleRHS)=
   let super = Option.map (fun i -> <:ctyp< 'cst $id:i$.$lid:name$ >>) gram.super in
   let body = ruleRHS gram super l in
   (*This is the actual constraint 'cst=<$constr$; ..>*)
   let constr = [ <:ctyp< 'cst >>, <:ctyp< < $constr$ ;.. > >> ] in
   let ty=Ast.TyDcl (_loc, name , [ <:ctyp< 'cst >> ],body,constr) in
   <:str_item< type $ty$>>;;

  (*w
    [@OpenType.gen@] Generates the open types for a given grammar. A more
    detailled description of this transformation can be found [[#OpenType|here]]
  *)
  let gen (gram:grammar)=
  (*
   The constraint for the type parameter which will be used
   to close the reccursion
  *)
  let constr =
   G.fold begin fun typ _ objectType ->
    <:ctyp< $lid:typ$ : '$lid:typ$ ; $objectType$>>
   end gram <:ctyp< >>
  in
  G.fold begin fun s l acc ->
   <:str_item< $acc$ $rule constr gram s l$ >>
  end gram <:str_item< >>;;

  (*w
    Generate the object type used to close a open type. That is, assuming we
    have a grammar containing three rules: ^^a^^,^^b^^ and ^^c^^ this will
    generate the following type:
    %%
    <a:a;b:b;c:c>
    %%
  *)
  let loopbackType gram =
   G.fold begin fun typ _ objectType ->
    <:ctyp< $lid:typ$ : $lid:typ$ ; $objectType$ >>
   end gram <:ctyp<>>

  (*w [@OpenType.close@] Close a previously defined open type. A more detailled
    description of this transformation can be found [[#OpenType|here]] *)
  let close gram gramId=
   let trans s=
    Ast.TyDcl (_loc, s ,[],<:ctyp< cst $id:gramId$.$lid:s$>> , [])
   in
   let closedDecls=
    G.fold begin fun s _ acc ->
     <:ctyp< $acc$ and $trans s$ >>
    end gram <:ctyp< >>
  (*The loopback constraint...*)
   in
<:str_item< type cst = < $loopbackType gram$ >
            and $closedDecls$>>
 end

(*w ==Mappers...==

  The following set of functions are used to define the traversal function. We
  are providing only one mean of traversal: a monadic map. We (naïvely) hope
  this is powerfull enough to capture most of the other transformations one
  could yearn for.
*)
module Mappers:
sig
 val gen:grammar -> Ast.str_item
end =
struct
 let rec genIt gram=
  let self s =genIt gram s
  and genCom = genCom gram in
  function
   | Atom s when G.mem s gram -> <:expr< T.$lid:s$ >>
   | Atom _ -> <:expr< return >>
   | List it -> <:expr< mmap $self it$>>
   | Tup l ->
      let pat,process = genCom (fun i -> <:expr< $i$ >>) l
      in <:expr< fun $pat$ -> $process$ >>
   | Option it ->
      let pat,process = genCom (fun i -> <:expr< Some $i$ >>) [it]in
      <:expr<function
       | None -> return None
       | Some $pat$ -> $process$
          >>

 and genCom gram genRet l =
  let elems = List.map (fun ty -> (fresh ()),ty ) l in
  let idPats = List.map (fun (i,_) -> <:patt< $id:i$ >>) elems in
  let retEl = List.map (fun (i,_) -> <:expr< $id:i$ >>) elems in
  let process = genBinders gram
   <:expr< return ($genRet (exList2ExCom retEl)$) >> elems
  in
  pattList2Pattern idPats,process


 (**
    Generates all the intermediare definitions for a variable
 *)
 and genBinders gram expr =
  let self l = genBinders gram expr l in
  function
   | (_,Atom s)::l when not (G.mem s gram) ->
      (*
        This item is not part of the grammar, it doesn't need to be binded
      *)
      self l
   | (id,ri)::l ->
      <:expr< Mon.bind ($genIt gram ri$ $id:id$) ( fun $id:id$ -> $self l$ )  >>
   | [] -> expr

 and genRule gram name= function
  | Other s -> <:match_case< #$lid:s$ as v -> return v >>
  | Labeled (s,[]) -> <:match_case< `$s$ -> return `$s$ >>
  | Labeled (s,rl) ->
     let pat,process=genCom gram
      (fun i -> <:expr< `$s$ $i$>>)
      rl
     in
     let pat = Ast.PaApp (_loc,<:patt< `$s$ >>,pat) in
     <:match_case< $pat$ -> $process$ >>
  | Super ->
     <:match_case< #Old.$lid:name$ as i ->
   Mon.bind ($lid:name$ i) ( fun i -> return (i :> (o Gram.$lid:name$)))
   >>

 let genFunc gram name=function
  | Abstract -> <:str_item< let $lid:name$ _ = assert false >>
  | Import -> <:str_item< >>
  | Alias it -> <:str_item< let $lid:name$ = $genIt gram it$ >>
  | Variant l ->
     let cases = List.map (genRule gram name) l in
     <:str_item< let $lid:name$ = function $matchcaseList2matchcase cases$
       >>

 let gen gram=
  G.fold
   begin
    fun name l acc -> <:str_item<
     $genFunc gram name l$;;
   $acc$>>
   end gram <:str_item< >>

end

EXTEND Gram
 GLOBAL: str_item;

 str_item: LEVEL "top" [
  [ "gram"; e = extends ; "{" ; it=gram_items ; "}" ->
   let super = Option.map (fun i -> <:ident< $i$.Gram >>) e in
   let gram=G.init super it in
   <:str_item<
    (*The open grammar*)
    module Gram = struct
     $OpenType.gen gram$
    end;;
    $OpenType.close gram <:ident<Gram>>$
    (*The mapper*)
   module Trav =
   struct
    module type AstDef=
    sig
     $G.fold
      begin
       fun i _ acc ->
        <:sig_item< type $lid:i$ ;; $acc$ >>
      end gram <:sig_item< >>
      $
    end;;
    module GetConstr(A:AstDef)=
    struct
     open A
     type t = < $OpenType.loopbackType gram$ >
    end;;
    module Conv(From:AstDef)(To:AstDef)(Mon:Monad.T)=
    struct
     (**Monadic functions...**)
     module MonHelp=Monad.Helper(Mon);;
     open MonHelp;;
     $match e with
      | None -> <:str_item< >>
      | Some i ->
          let conv = <:ident< $id:i$.Trav.Conv >> in
          <:str_item<module Super = $id:conv$(From)(To)(Mon) >>
          $;;

     (*The whole tree is translated from the first ast to the second*)
      module type Par =
      sig
       $G.fold
        begin
         fun i _ acc ->
          <:sig_item<
        val $i$ : From.$lid:i$ -> To.$lid:i$ m;;
        $acc$
        >>
        end gram <:sig_item< >>$
      end

      type i=GetConstr(From).t
      type o=GetConstr(To).t

      (*The whole tree is translated From -> To excepted the toplevel node*)
      module type T =
      sig
       open Mon
       open Gram
        $G.fold
        begin
         fun i _ acc -> <:sig_item<
        val $i$ : i $lid:i$ -> o $lid:i$ m;;
        $acc$>>
        end gram <:sig_item< >>$
      end

       $ match gram.super with
        | None -> <:str_item< >>
        | Some super -> <:str_item< module Old = struct
           $G.fold
            begin
             fun i _ acc ->
              <:str_item<
            type $lid:i$ = i $id:super$.$lid:i$;;
            $acc$
            >>
            end gram <:str_item< >>$
          end >>$;;

      module type Ext=functor (T:Par) -> T

      (*Performs a monadic traversal of the tree...*)
      module Base:Ext=
       functor(T:Par) ->
      struct
       $let st =match e with
        | None -> <:str_item< >>
        | Some _ -> <:str_item<include Super.Base(T) >>
      in
      <:str_item< $st$;; $Mappers.gen gram$>>
       $
    end
 end

    (*Closes the loop usefull to define easilly transformation within the same ast*)
    module Map(Mon:Monad.T)=
    struct
     (*This grammar*)
     module Self=
     struct
      $let s =
       G.fold begin
        fun s _ acc-> <:str_item< $acc$ ;; type $lid:s^"_loop"$ = $lid:s$ >>
       end gram <:str_item< >>
       in
       G.fold begin
        fun s _ acc -> <:str_item< $acc$ ;; type $lid:s$ = $lid:s^"_loop"$ >>
       end gram s
       $
     end
     include Conv(Self)(Self)(Mon)
     module Make(E:Ext):T=
     struct
      module rec T:T=E(T)
      include T
     end
    end;;
 end;;
>>
]
 ];
 gram_items:[
  [it = LIST0 [ gram_item ] SEP ";" ; OPT ";" -> List.flatten it]
 ];
 gram_item:[
  [ "import" ; l = LIST1 a_LIDENT SEP "," ->
     List.map  (fun s -> s,Import) l
  | id=a_LIDENT ; ":=" ; OPT "|" ; l = LIST1 rule_branch SEP "|" ->
     if List.for_all (fun x -> x==Super) l then
      [id,Import]
     else
      [id,Variant l]
  | id=a_LIDENT ; ":=" ; r =  rule_item ->
     [id,Alias r]
  | id=a_LIDENT ->
     [id,Abstract]
  ]
 ];
 rule_item:[
  [id = a_LIDENT ->
    Atom id
  | "["; i = rule_item ;"]" ->
     List i
  | "["; l = LIST1 rule_item SEP "," ;"]" ->
     List (Tup l)
  | "("; l = LIST1 rule_item SEP "," ;")" ->
     Tup l
  |  i = rule_item ; "?" ->
     Option i
  ]
 ];
 rule_branch:[
  [ "`";id = a_UIDENT; l = LIST0 rule_item SEP "," -> Labeled (id,l)
  | "super" -> Super
  | id = a_LIDENT -> Other id
  ]
 ];
 uid:[
  [i = LIST1 a_UIDENT SEP "." ->
    let rec uid2ident = function
     | [a]-> <:ident<$uid:a$>>
     | a::b -> <:ident<$uid:a$.$uid2ident b$>>
     | [] -> assert false
    in
    uid2ident i
  ]
 ];

 (*grammar inheritence...*)
 extends:[
  ["extends"; i=uid  ->
    Some <:ident< $i$ >>
  | ->
     None
  ]
 ];

END
