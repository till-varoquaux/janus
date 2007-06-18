(*w
  ====Ast pretty printers====

  This is the base module of all our pretty printers
*)

(*w ==Formatter module==
  We are using the Pp module from Christian Liding
*)
include Pp

(*w
  This indicates a point where we can break a line when formatting.
*)
let breakNull=breakWith ""

(*w
  Pretty print a list of elements. Takes the list of elements to pretty print,
  the function used to pretty an element and the separator to insert in beetween
  pretty printed elements.
*)
let join (conv:'a -> doc) (l:'a list) (sep:doc):doc=
 let first=ref true in
 let aux beg el =
  match (conv el),!first with
   | c,_ when c=empty -> empty
   | c,true ->
      first:=false;
      c
   | c,false ->
      beg ^^ sep ^^ c
 in
 List.fold_left aux empty l

open General

let sprintf=Printf.sprintf

(*w
  ==Pretty printing style==

  This module adds formatting information (decoration).
*)
module Style=
struct
 type weight=
  | Normal
  | Bold
  | Underscore
  | Blink
  | Reverse
  | Concealed

 let decodeWeight=function
  | Normal -> 0
  | Bold -> 1
  | Underscore -> 2
  | Blink -> 3
  | Reverse -> 4
  | Concealed -> 5

 type color=
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Pink
  | LightBlue
  | White

 let decodeColor=function
  | Black -> 30
  | Red -> 31
  | Green -> 32
  | Yellow -> 33
  | Blue -> 34
  | Pink -> 35
  | LightBlue -> 36
  | White -> 37

 let tty=Unix.isatty Unix.stdout

 let print ?color ?weight txt=
  if tty then
   let weight=
   Option.map_default
    begin
     fun w -> sprintf "%.1u" (decodeWeight w)
    end "" weight
   and color=
    Option.map_default
     begin
      fun c -> sprintf "%.2u" (decodeColor c)
     end "" color
   in
   let col=sprintf "\027[%s;%sm" weight color in
   (nullText col)^^(text txt)^^(nullText "\027[0m")
  else
   text txt

 let par p=
  print ~color:Red p

 let punct p=
  print ~color:Red p

 let kwd k=
  print ~color:Blue ~weight:Bold k

 let bool c=
  print ~color:Pink c

 let ident i=
  print i

 let string s=
  print ~color:Green s

 let number n=
  print ~color:Pink n

 let op o=
  print ~color:Red o

 let lit l=
  print ~color:Yellow l
end

include Style

let par s =
 (par "(") ^^ s ^^ (par ")")

and brace s =
 (par "{") ^^ s ^^ (par "}")

and bracket s =
 (par "[") ^^ s ^^ (par "]")

(*This is just used to unsure we do NOT use ^^text^^ in the functions below...*)
type abstract
let text : abstract = Obj.magic ()

(*w
  ==Monadic traversal==
*)
module PrinterMonad=
struct
 exception NotImplemented
 type 'a m=Pp.doc
 let return _ = raise NotImplemented
 let bind _ = raise NotImplemented
end

(*w
  A weak set of formated text
*)
module Ws=
 Weak.Make(
  struct
   type t=Pp.doc
   let equal=(=)
   let hash=Hashtbl.hash
  end
 )

(*w
  A bunch of convenience functions required by all the printers.
*)
module type ConvIn=
sig
 module In:
  sig
   type expr
   type instr
  end
 val instr:In.instr -> Pp.doc
 val expr:In.expr -> Pp.doc
end

let blocs=Ws.create 17

module Convenience(S:ConvIn)=
struct
   let braceIndent s=
    vgrp(brace (vgrp(nest 4 (break^^s))^^break))

   let joinInstrs l=
    join S.instr l ((punct ";")^^break)

   let bloc l =
    let r=braceIndent (joinInstrs l) in
    Ws.add blocs r;
    r

   let protectInstr i =
    let r=S.instr i in
    if Ws.mem blocs r then
     r
    else
     braceIndent r

   let cond e=
    par (S.expr e)

   let rec blocOrInstr ?(breakAfter=false) i =
    let i'=S.instr i in
    if Ws.mem blocs i' then
     i'
    else if breakAfter then
     vgrp(nest 4 (break^^i')^^ break)
    else
     vgrp(nest 4 (break^^i'))

   (*w
     This function escapes Strings to be printed as javascript strings
   *)
   let escape s=
    let b=Buffer.create (String.length s) in
    let a= Buffer.add_string b in
    let process=function
     | '\t' -> a "\\t"
     | '\n' -> a "\\n"
     | '\"' -> a "\\\""
     | '\\' -> a "\\\\"
     | c -> Buffer.add_char b c
    in
    String.iter process s;
    Buffer.contents b
end
