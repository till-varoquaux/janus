open General
open Camlp4.PreCast
open Syntax
open General
open Camlp4Helper
open Grammar.Types

(*w
 * Constrained types are type with a set of constraints. The set contains
 * only strings since all contraints bind a string ^^s^^ to the type
 * parameter ^^'s^^.
 *)
type constrainedType=Ast.ctyp*String.Set.t

(*w
 * Generates the type for a ruleItem.
 *)
let rec ruleItem _loc gram : ruleItem -> constrainedType = function
  | Option r ->
      let t,cst=ruleItem _loc gram r in
      <:ctyp< $t$ option >>,cst
  | Tup l -> ruleItems _loc gram l
  | Atom s when Grammar.mem s gram ->
      <:ctyp< '$lid:s$ >>,(String.Set.singleton s)
  | Atom s -> <:ctyp< $lid:s$ >>,String.Set.empty
  | Nativ ty -> ty,String.Set.empty
  | List l -> let t,cst=ruleItem _loc gram l in <:ctyp< $t$ list >>,cst

and ruleItems _loc gram : ruleItem list -> constrainedType = function
  | [] -> <:ctyp< >>,String.Set.empty
  | [r] -> ruleItem _loc gram r
  | l ->
      let t,cst=List.fold_left l
        ~init:(<:ctyp< >>,String.Set.empty)
       ~f:fun (tup,cst) t ->
        let t',cst'=ruleItem _loc gram t in
        (Ast.TySta (_loc,tup,t')),(String.Set.union cst cst')
      in
      <:ctyp< $tup:t$ >>,cst;;

let ruleBranch _loc gram super : branch -> constrainedType = function
  | Other s -> <:ctyp< $lid:s$ >>,String.Set.empty
  | Labeled (s,rl) ->
      let t,cst=ruleItems _loc gram rl in
      <:ctyp< `$s$ of $t$ >>,cst
  | Super ->
      let cst= String.Set.of_list (Grammar.typeNames gram) in
      Option.get super,cst

let ruleRHS _loc gram super : ruleRHS -> constrainedType=function
  | Abstract -> <:ctyp< >>,String.Set.empty
  | Import -> ruleBranch _loc gram super Super
  | Alias i -> ruleItem _loc gram i
  | Variant [] | PolVar [] -> assert false
  | Variant l ->
      List.fold_left l
        ~init:(<:ctyp< >>,String.Set.empty)
        ~f:fun (acc,cst) (lbl,rl) ->
          let body,cst'=ruleItems _loc gram rl in
          let t = <:ctyp< $uid:lbl$ of $body$ >> in
          <:ctyp< $acc$ | $t$ >>,String.Set.union cst cst'
  | PolVar rb ->
      let body,cst=
        List.fold_left rb
          ~init:(<:ctyp< >>,String.Set.empty)
          ~f:fun (acc,cst) s ->
            let b,cst'=ruleBranch _loc gram super s in
            <:ctyp< $acc$ | $b$ >>,String.Set.union cst cst'
      in
      <:ctyp< [ $body$ ] >>,cst;;

(*
 * Generates the open type def corresponding to one rule in the grammar.
 * constr is the constraint on the tye parameter.
 *)
let rule _loc gram name (l:ruleRHS)=
  let getSuperFun i = <:ctyp< 'cst $id:i$.Gram.$lid:name$ >> in
  let super = Option.map ~f:getSuperFun (Grammar.super gram) in
  let body,cset = ruleRHS _loc gram super l in
  (*This is the actual constraint 'cst=<$constr$; ..>*)
  let constr =
    if String.Set.is_empty cset then
      []
    else
      let c=
        String.Set.fold cset
          ~f:(fun typ objectType ->
                <:ctyp< $lid:typ$ : '$lid:typ$ ; $objectType$>>
             )
          ~init:<:ctyp< >>
      in
      [ <:ctyp< 'cst >>, <:ctyp< < $c$ ;.. > >> ]
  in
  let ty=Ast.TyDcl (_loc, name , [ <:ctyp< 'cst >> ],body,constr) in
  <:str_item< type $ty$>>;;

(*w
 * [@OpenType.gen@] Generates the open types for a given grammar. A more
 * detailled description of this transformation can be found
 * [[#OpenType|here]]
 *)
let gen (_loc:Loc.t) (gram:Grammar.t)=
  Grammar.assocs gram
  |> List.map ~f:(fun (name,v) -> rule _loc gram name v)
  |> Ast.stSem_of_list
(*  Grammar.fold gram
    ~init:<:str_item< >>
    ~f:(fun ~key ~data acc -> <:str_item< $acc$ $rule _loc gram key data$ >>)*)

(*w
 * Generate the object type used to close a open type. That is, assuming we
 * have a grammar containing three rules: ^^a^^,^^b^^ and ^^c^^ this will
 * generate the following type:
 * %%
 * <a:a;b:b;c:c>
 * %%
 *)
let loopbackType _loc gram gramId =
  <:ctyp< < $Grammar.typeNames gram
  |> List.map ~f:(fun s -> <:ctyp< $lid:s$:$id:gramId$.$lid:s$>>)
  |> Ast.tySem_of_list$ > >>

(*w
 * [@OpenType.close@] Close a previously defined open type. A more detailled
 * description of this transformation can be found [[#OpenType|here]]
 *)
let close _loc gram gramId=
  <:str_item<
   type cst = <
       $Grammar.typeNames gram
       |> List.map ~f:(fun s -> <:ctyp< $lid:s$:$lid:s$ >>)
       |> Ast.tySem_of_list$
    >
   and
       $Grammar.typeNames gram
       |> List.map ~f:(fun s -> tyDcl s <:ctyp< cst $id:gramId$.$lid:s$>>)
       |> Ast.tyAnd_of_list
  $>>
