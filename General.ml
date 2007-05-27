(*w
  This file contains many genral utility functions.
*)
module List=
 struct
  include List
  let scan a l =
   let rec aux i = function
    | [] -> raise Not_found
    | x::_ when x=a -> i
    | _::t -> aux (i+1) t
   in
   aux 0 l
 end

(*
  TODO: This should be moved elsewhere...
*)
type ty=[
| `T (*Base type*)
| `Arrow of ty list*ty (*Normal application*)
| `CpsArrow of ty list*ty] (*Cps function*)

type ty'=[
| ty
| `Macro of AstBase.macrobloc*(ty list)
| `CpsMacro of AstBase.macrobloc*(ty list)]

open Lexing
type location = Lexing.position * Lexing.position

let locToString (p1,p2) =
 let col p =
  p.pos_lnum,(p.pos_cnum-p.pos_bol)
 in
 let line1,col1=col p1
 and line2,col2=col p2
 in
 if line1<>line2 then
  Printf.sprintf "line %i,%i-line %i,%i" line1 col1 line2 col2
 else
  Printf.sprintf "line %i,%i-%i" line1 col1 col2

type 'a pos = { node:'a; loc:location}

let dummyId i=
 {loc=Lexing.dummy_pos,Lexing.dummy_pos;node=i}

exception Error of string
exception UnlocError of string

let error ?pos s=
 match pos with
  | Some p -> raise (Error (Printf.sprintf "at %s,%s\n" (locToString p) s))
  | None -> raise (UnlocError s)

let protect f (`Pos(p,e)) =
 try(
  f e
 )with UnlocError s ->(
  error ~pos:p s
 )
