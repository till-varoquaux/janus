(*w
 * ==Camlp4==
 * A bunch of generic functions usefull to create nodes in camlp4
 *)
open Camlp4.PreCast
open Syntax
open Ast
open General

let ghost = Loc.ghost

(*w
 *  Generates a fresh id..
 *)
let fresh =
 let cpt = ref 0 in
 fun () ->
  incr cpt;
  <:ident@ghost< $lid:Printf.sprintf "id_%i" !cpt$ >>

(*w
 *  Transforms an expression list to a tupple of the given expressions.
 *)
let exList2ExCom= function
 | [] -> <:expr@ghost< >>
 | [e] -> e
 | (h::_) as l ->
     let loc = loc_of_expr h in
     <:expr@loc< $tup:Ast.exCom_of_list l$ >>

(*w
 * Transforms a  pattern list to a tupple of the given patterns.
 *)
let pattList2PatCom = function
 | [] -> <:patt@ghost< >>
 | [i] -> i
 | (h::_) as l ->
     let loc = loc_of_patt h in
     <:patt@loc< $tup:Ast.paCom_of_list l$ >>

let tyDcl ?(param=[]) ?(constraints=[]) name ty =
  let _loc = Ast.loc_of_ctyp ty in
  Ast.TyDcl (_loc, name ,param,ty,constraints)
