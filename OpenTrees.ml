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

open General
open Camlp4.PreCast
open Syntax
open Ast
module Options=Camlp4.Options
(*module L=ListLabels*)

(*w
  Syntax extension options...
*)
let noMap=ref false
let _= Options.add "-no_map" (Arg.Set noMap) "Do not generate mapper module"

(*w
  ==Camlp4==
  A bunch of generic functions usefull to create nodes in camlp4
*)

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
    let r=List.fold_left l
     ~init:<:expr< >>
     ~f:fun tup t ->Ast.ExCom (_loc,tup,t)
    in
    <:expr< $tup:r$ >>

(*w
  Transforms a  pattern list to a tupple of the given patterns.
*)
let pattList2Pattern = function
 | [] -> <:patt< >>
 | [i] -> i
 | l ->
    let r =List.fold_left l
     ~init:<:patt< >>
     ~f:fun tup i ->Ast.PaCom (_loc,tup,i)
    in
    <:patt< $tup:r$ >>

(*w
  Converts a list of match cases to the matchcase corresponding to the sum of
  all the given match cases.
*)
let matchcaseList2matchcase l =
 List.fold_left l
  ~init:<:match_case< >>
  ~f:fun acc s -> <:match_case< $acc$ | $s$ >>

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
 | Variant of (string*ruleItem list) list
 | PolVar of branch list
 | Alias of ruleItem
and branch =
  (*w This \\has\\ to be another polymorphic variant type *)
 | Other of string
 | Labeled of string * ruleItem list
 | Super
and ruleItem =
 | Option of ruleItem
 | Atom of string
 | Tup of ruleItem list
 | List of ruleItem

(*w
  This module is just a set of rules to ease the handling of grammars, and make
  them more abstract.
*)
module ExtGram=
 struct

  let mem key g =
   StringMap.mem key g.rules

  let fold g ~init ~f =
   StringMap.fold g.rules ~init:init ~f:f

  let init super gramItems=
   {
    super=super;
    rules=
     List.fold_left
     gramItems
     ~init:StringMap.empty
      ~f:fun m (k,v) -> StringMap.add m ~key:k ~data:v
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
  module C=Set.Make(String)
   (*w
     Constrained types are type with a set of constraints. The set contains only
     strings since all contraints bind a string ^^s^^ to the type parameter ^^'s^^.
   *)
  type constrainedType=Ast.ctyp*C.t

  (*w
    Generates the type for a ruleItem.
  *)
  let rec ruleItem gram : ruleItem -> constrainedType = function
   | Option r ->
      let t,cst=ruleItem gram r in
      <:ctyp< $t$ option >>,cst
   | Tup l -> ruleItems gram l
   | Atom s when G.mem s gram -> <:ctyp< '$lid:s$ >>,(C.singleton s)
   | Atom s -> <:ctyp< $lid:s$ >>,C.empty
   | List l -> let t,cst=ruleItem gram l in <:ctyp< $t$ list >>,cst

  and ruleItems gram : ruleItem list -> constrainedType = function
   | [] -> <:ctyp< >>,C.empty
   | [r] -> ruleItem gram r
   | l ->
      let t,cst=List.fold_left l
       ~init:(<:ctyp< >>,C.empty)
       ~f:fun (tup,cst) t ->
        let t',cst'=ruleItem gram t in
        (Ast.TySta (_loc,tup,t')),(C.union cst cst')
      in
      <:ctyp< $tup:t$ >>,cst;;

  let ruleBranch gram super : branch -> constrainedType = function
   | Other s -> <:ctyp< $lid:s$ >>,C.empty
   | Labeled (s,rl) ->
      let t,cst=ruleItems gram rl in
      <:ctyp< `$s$ of $t$ >>,cst
   | Super ->
      let cst=G.fold gram
       ~init:C.empty
       ~f:(fun ~key ~data:_ cst -> C.add key cst)
      in
      Option.get super,cst

  let ruleRHS gram super : ruleRHS -> constrainedType=function
   | Abstract -> <:ctyp< >>,C.empty
   | Import -> ruleBranch gram super Super
   | Alias i -> ruleItem gram i
   | Variant ([]) | PolVar [] -> assert false
   | Variant l ->
      List.fold_left l
       ~init:(<:ctyp< >>,C.empty)
       ~f:fun (acc,cst) (lbl,rl) ->
        let body,cst'=ruleItems gram rl in
        let t = <:ctyp< $uid:lbl$ of $body$ >> in
        <:ctyp< $acc$ | $t$ >>,C.union cst cst'
   | PolVar rb ->
      let body,cst=
       List.fold_left rb
        ~init:(<:ctyp< >>,C.empty)
        ~f:fun (acc,cst) s ->
         let b,cst'=ruleBranch gram super s in
         <:ctyp< $acc$ | $b$ >>,C.union cst cst'
      in
      <:ctyp< [ $body$ ] >>,cst;;

  (*
    Generates the open type def corresponding to one rule in the grammar.
    constr is the constraint on the tye parameter.
  *)
  let rule gram name (l:ruleRHS)=
   let super = Option.map (fun i -> <:ctyp< 'cst $id:i$.Gram.$lid:name$ >>) gram.super in
   let body,cset = ruleRHS gram super l in
   (*This is the actual constraint 'cst=<$constr$; ..>*)
   let constr =
    if C.is_empty cset then
     []
    else
     let c=
      C.fold begin fun typ objectType ->
       <:ctyp< $lid:typ$ : '$lid:typ$ ; $objectType$>>
      end cset <:ctyp< >>
     in
     [ <:ctyp< 'cst >>, <:ctyp< < $c$ ;.. > >> ]
   in
   let ty=Ast.TyDcl (_loc, name , [ <:ctyp< 'cst >> ],body,constr) in
   <:str_item< type $ty$>>;;

  (*w
    [@OpenType.gen@] Generates the open types for a given grammar. A more
    detailled description of this transformation can be found [[#OpenType|here]]
  *)
  let gen (gram:grammar)=
   G.fold gram
    ~init:<:str_item< >>
    ~f:(fun ~key ~data acc -> <:str_item< $acc$ $rule gram key data$ >>)

  (*w
    Generate the object type used to close a open type. That is, assuming we
    have a grammar containing three rules: ^^a^^,^^b^^ and ^^c^^ this will
    generate the following type:
    %%
    <a:a;b:b;c:c>
    %%
  *)
  let loopbackType gram =
   G.fold gram
    ~init:<:ctyp< >>
    ~f:(fun ~key:typ ~data:_ objectType ->
         <:ctyp< $lid:typ$:$lid:typ$;$objectType$ >>)

  (*w [@OpenType.close@] Close a previously defined open type. A more detailled
    description of this transformation can be found [[#OpenType|here]] *)
  let close gram gramId=
   let trans s=
    Ast.TyDcl (_loc, s ,[],<:ctyp< cst $id:gramId$.$lid:s$>> , [])
   in
   let closedDecls=
    G.fold gram
     ~init:<:ctyp< >>
     ~f:(fun ~key:s ~data:_ acc ->
          <:ctyp< $acc$ and $trans s$ >>)
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

  The traversal is done using reccursive modules: TODO (finish the explanations)
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
  let elems = List.map l ~f:fun ty -> (fresh ()),ty in
  let idPats = List.map elems ~f:fun (i,_) -> <:patt< $id:i$ >> in
  let retEl = List.map elems ~f:fun (i,_) -> <:expr< $id:i$ >> in
  let process = genBinders gram
   <:expr< return ($genRet (exList2ExCom retEl)$) >> elems
  in
  pattList2Pattern idPats,process

 (*w
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
     <:match_case< #In.$lid:name$ as i ->
   Mon.bind ($lid:name$ i) ( fun i -> return (i :> (o Gram.$lid:name$)))
   >>

 and genVariant gram = function
  | s,[] -> <:match_case< $uid:s$ -> return $uid:s$ >>
  | s,rl ->
     let pat,process=genCom gram
      (fun i -> <:expr< $uid:s$ $i$>>)
      rl
     in
     let pat = Ast.PaApp (_loc,<:patt< $uid:s$ >>,pat) in
     <:match_case< $pat$ -> $process$ >>

 let genFunc gram name=function
  | Abstract -> <:str_item< let $lid:name$ _ = assert false >>
  | Import -> <:str_item< >>
  | Alias it -> <:str_item< let $lid:name$ = $genIt gram it$ >>
  | Variant l ->
     let cases = List.map l ~f:(genVariant gram) in
     <:str_item< let $lid:name$ = function $matchcaseList2matchcase cases$
       >>
  | PolVar l ->
     let cases = List.map l ~f:(genRule gram name) in
     <:str_item< let $lid:name$ = function $matchcaseList2matchcase cases$
       >>

 let gen gram=
  G.fold gram
   ~init:<:str_item< >>
   ~f:(fun ~key:name ~data:l acc -> <:str_item<
        $genFunc gram name l$;;
       $acc$>>)

end

let genTrav gram=
 <:str_item<
  (*The mapper*)
    module type AstDef=
    sig
     $G.fold gram
      ~init:<:sig_item< >>
      ~f:fun ~key:i ~data:_ acc ->
       <:sig_item< type $lid:i$ ;; $acc$ >>
      $
    end;;
    module GetConstr(A:AstDef)=
    struct
     open A
     type t = < $OpenType.loopbackType gram$ >
    end;;
    module Conv(From:AstDef)(To:AstDef)(Mon:Monad.T)=
    struct
     (*Monadic functions...**)
     module MonHelp=Monad.Helper(Mon);;
     open MonHelp;;
     $match gram.super with
      | None -> <:str_item< >>
      | Some i ->
          let conv = <:ident< $id:i$.Trav.Conv >> in
          <:str_item<module Super = $id:conv$(From)(To)(Mon) >>
          $;;

      (*w
        This is a copy of the module specifying the input type (^^From^^)
        residing at the type level.

        We shall use it it latter to define a copy of (^^From^^)...
        %%
module type C=
sig
 type t=int
end;;

module B(C:C)=
 struct
  include C
 end;;

module rec D:C=B(D);;
%%
        This actually works in OCaml since types are statically resolved using
        the type level information. This might not be the case in future versions
        of OCaml. If this ever happens we will need to use another
        technique. This is nothing we can't get do.
      *)
      module type In =
      sig
       $G.fold gram
        ~init:<:sig_item< >>
        ~f:(fun ~key:i ~data:_ acc -> <:sig_item<
            type $lid:i$ = From.$lid:i$;;
            $acc$>>)$
      end


      (*w
        This is a copy of the output module residing at the type level (^^To^^).
      *)
      module type Out =
      sig
       $G.fold gram
        ~init:<:sig_item< >>
        ~f:(fun ~key:i ~data:_ acc -> <:sig_item<
              type $lid:i$ = To.$lid:i$;;
              $acc$>>)$
      end

     (*w
       Represents a full transformation of the tree from the types defined in
       ^^From^^ to the types defined in ^^To^^.
     *)
      module type Translation =
      sig
       $G.fold gram
        ~init:<:sig_item< >>
        ~f:(fun ~key:i ~data:_ acc ->
               <:sig_item<
              val $i$ : From.$lid:i$ -> To.$lid:i$ m;;
              $acc$
              >>)$
      end

     (*w
       These are the object types used to close the loop in ^^From^^ and
       ^^To^^. They are used to define partial transformation's type.
     *)
      type i=GetConstr(From).t
      type o=GetConstr(To).t

      (*w
        The whole tree is translated From -> To excepted the root which is kept
        identical. In case of a Map partial transformations and full
        transformations are indentical.
      *)
      module type PartialTranslation =
      sig
       open Gram
        $G.fold gram
        ~init:<:sig_item< >>
        ~f:(fun ~key:i ~data:_ acc -> <:sig_item<
            val $i$ : i $lid:i$ -> o $lid:i$ m;;
            $acc$>>)$
      end

       $ match gram.super with
        | None -> <:str_item< >>
        | Some super -> <:str_item< module In = struct
           $G.fold gram
            ~init:<:str_item< >>
            ~f:(fun ~key:i ~data:_ acc ->
                 <:str_item<
                type $lid:i$ = i $id:super$.Gram.$lid:i$;;
                $acc$>>)$
          end >>$;;

      module type Ext=functor (T:Translation) -> PartialTranslation

      (*Performs a monadic traversal of the tree...*)
      module Base:Ext=
       functor(T:Translation) ->
      struct
       $let st =match gram.super with
        | None -> <:str_item<
           >>
        | Some _ -> <:str_item<include Super.Base(T) >>
      in
      <:str_item< $st$;; $Mappers.gen gram$>>
       $
    end
   end

   module TranslateFrom(From:AstDef)(Mon:Monad.T)=
   struct
    include Conv(From)(ClosedDef)(Mon)
    module type TF=functor(T:Translation) -> Translation
    module Make(E:TF)=
    struct
     module rec T:Translation=E(T)
     include T
    end
   end

   (*Closes the loop. This usefull to define easilly transformation within the same ast*)
   module Map(Mon:Monad.T)=
   struct
    include Conv(ClosedDef)(ClosedDef)(Mon)
    module Make(E:Ext):Translation=
    struct
     module rec T:Translation=E(T)
     include T
    end
   end
 >>

EXTEND Gram
 GLOBAL: str_item;

 str_item: LEVEL "top" [
  [ "gram"; name = OPT a_UIDENT;e = extends ; "{" ; it=gram_items ; "}" ->
   let gram=
    G.init e it
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
]
 ];
 gram_items:[
  [it = LIST0 [ gram_item ] SEP ";" ; OPT ";" -> List.flatten it]
 ];
 gram_item:[
  [ "import" ; l = LIST1 a_LIDENT SEP "," ->
     List.map  l ~f:(fun s -> s,Import)
  | id=a_LIDENT ; ":=" ; OPT "|" ; l = LIST1 variant SEP "|" ->
     [id,Variant l]
  | id=a_LIDENT ; ":=" ; OPT "|" ; l = LIST1 rule_branch SEP "|" ->
     if List.for_all l ~f:(fun x -> x==Super) then
      [id,Import]
     else
      [id,PolVar l]
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
 variant: [
  [id = a_UIDENT; l = LIST0 rule_item SEP "," -> (id,l)]
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
