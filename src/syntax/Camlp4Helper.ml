(*w
 * ==Camlp4==
 * A bunch of generic functions usefull to create nodes in camlp4
 *)
open Camlp4.PreCast
open Syntax
open Ast
open General

let _loc = Loc.ghost

(*w
 *  Generates a fresh id..
 *)
let fresh =
 let cpt = ref 0 in
 fun () ->
  incr cpt;
  <:ident< $lid:Printf.sprintf "id_%i" !cpt$ >>

(*w
 *  Transforms an expression list to a tupple of the given expressions.
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
 * Transforms a  pattern list to a tupple of the given patterns.
 *)
let pattList2Pattern = function
 | [] -> <:patt< >>
 | [i] -> i
 | l ->
    let r =List.fold_left l
     ~init:<:patt< >>
     ~f:fun tup i -> Ast.PaCom (_loc,tup,i)
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

