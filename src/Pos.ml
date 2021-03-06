(*w
 * ====Positions and error handling====
 * This module handles position in a source tree and the reporting of errors
 *)
open Lexing
type location = Lexing.position * Lexing.position

let locToString (p1,p2) =
 let file=p1.pos_fname in
 let col p =
  p.pos_lnum,(p.pos_cnum-p.pos_bol)
 in
 let line1,col1=col p1
 and line2,col2=col p2
 in
 if line1<>line2 then
  Printf.sprintf "File \"%s\",line %i,%i-line %i,%i" file line1 col1 line2 col2
 else
  Printf.sprintf "File \"%s\",line %i,%i-%i" file line1 col1 col2

type 'a pos = { node:'a; loc:location}

let unPos {node=i;loc=_} = i

let dummyId i=
 {loc=Lexing.dummy_pos,Lexing.dummy_pos;node=i}

exception Error of string
exception UnlocError of string

let error ?pos s=
 match pos with
  | Some p -> raise (Error (Printf.sprintf "at %s,%s\n" (locToString p) s))
  | None -> raise (UnlocError s)

(*w
 * This decorates unlocalized errors with some positionnal information
 *
 * **TODO** Needs a refactoring
 *)
let protect f (`Pos(p,e)) =
 try(
  f e
 )with UnlocError s ->(
  error ~pos:p s
 )
