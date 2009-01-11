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
let rec ruleItem gram : ruleItem -> constrainedType = function
  | Option r ->
      let t,cst=ruleItem gram r in
      <:ctyp< $t$ option >>,cst
  | Tup l -> ruleItems gram l
  | Atom s when Grammar.mem s gram ->
      <:ctyp< '$lid:s$ >>,(String.Set.singleton s)
  | Atom s -> <:ctyp< $lid:s$ >>,String.Set.empty
  | Nativ ty -> ty,String.Set.empty
  | List l -> let t,cst=ruleItem gram l in <:ctyp< $t$ list >>,cst

and ruleItems gram : ruleItem list -> constrainedType = function
  | [] -> <:ctyp< >>,String.Set.empty
  | [r] -> ruleItem gram r
  | l ->
      let t,cst=List.fold_left l
        ~init:(<:ctyp< >>,String.Set.empty)
       ~f:fun (tup,cst) t ->
        let t',cst'=ruleItem gram t in
        (Ast.TySta (_loc,tup,t')),(String.Set.union cst cst')
      in
      <:ctyp< $tup:t$ >>,cst;;

let ruleBranch gram super : branch -> constrainedType = function
  | Other s -> <:ctyp< $lid:s$ >>,String.Set.empty
  | Labeled (s,rl) ->
      let t,cst=ruleItems gram rl in
      <:ctyp< `$s$ of $t$ >>,cst
  | Super ->
      let cst=Grammar.fold gram
        ~init:String.Set.empty
        ~f:(fun ~key ~data:_ cst -> String.Set.add key cst)
      in
      Option.get super,cst

let ruleRHS gram super : ruleRHS -> constrainedType=function
  | Abstract -> <:ctyp< >>,String.Set.empty
  | Import -> ruleBranch gram super Super
  | Alias i -> ruleItem gram i
  | Variant [] | PolVar [] -> assert false
  | Variant l ->
      List.fold_left l
        ~init:(<:ctyp< >>,String.Set.empty)
        ~f:fun (acc,cst) (lbl,rl) ->
          let body,cst'=ruleItems gram rl in
          let t = <:ctyp< $uid:lbl$ of $body$ >> in
          <:ctyp< $acc$ | $t$ >>,String.Set.union cst cst'
  | PolVar rb ->
      let body,cst=
        List.fold_left rb
          ~init:(<:ctyp< >>,String.Set.empty)
          ~f:fun (acc,cst) s ->
            let b,cst'=ruleBranch gram super s in
            <:ctyp< $acc$ | $b$ >>,String.Set.union cst cst'
      in
      <:ctyp< [ $body$ ] >>,cst;;

(*
 * Generates the open type def corresponding to one rule in the grammar.
 * constr is the constraint on the tye parameter.
 *)
let rule gram name (l:ruleRHS)=
  let getSuperFun i = <:ctyp< 'cst $id:i$.Gram.$lid:name$ >> in
  let super = Option.map getSuperFun (Grammar.super gram) in
  let body,cset = ruleRHS gram super l in
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
let gen (gram:Grammar.t)=
  Grammar.fold gram
    ~init:<:str_item< >>
    ~f:(fun ~key ~data acc -> <:str_item< $acc$ $rule gram key data$ >>)

(*w
 * Generate the object type used to close a open type. That is, assuming we
 * have a grammar containing three rules: ^^a^^,^^b^^ and ^^c^^ this will
 * generate the following type:
 * %%
 * <a:a;b:b;c:c>
 * %%
 *)
let loopbackType gram gramId =
  <:ctyp< < $
    Grammar.fold gram
      ~init:<:ctyp< >>
      ~f:(fun ~key:typ ~data:_ objectType ->
            <:ctyp< $lid:typ$:$id:gramId$.$lid:typ$;$objectType$ >>) $ > >>

(*w
 * [@OpenType.close@] Close a previously defined open type. A more detailled
 * description of this transformation can be found [[#OpenType|here]]
 *)
let close gram gramId=
  let trans s=
    Ast.TyDcl (_loc, s ,[],<:ctyp< cst $id:gramId$.$lid:s$>> , [])
  in
  let closedDecls=
    Grammar.fold gram
      ~init:<:ctyp< >>
      ~f:(fun ~key:s ~data:_ acc ->
            <:ctyp< $acc$ and $trans s$ >>)
      (*The loopback constraint...*)
  in
    <:str_item< type cst = < $Grammar.fold gram
                    ~init:<:ctyp< >>
                    ~f:(fun ~key:typ ~data:_ objectType ->
                          <:ctyp< $lid:typ$:$lid:typ$;$objectType$ >>)
                    $ >
                and $closedDecls$>>
