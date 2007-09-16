(*w
 * ====Ast pretty printers====
 *
 * This is the base module of all our pretty printers. Pretty-printing is
 * separated in two phase: indenting and syntax highlighting. The syntax
 * highlighting phase uses a "formatter" wich can be swapped at
 * runtime. Formatters take care of the device dependant part of the pretty
 * printing such as escaping characters or adding colors.
 *)

(*w ==Formatter module==
 * We are using the Pp module from Christian Liding
 *)
include Pp
open General

(*w
 *  This indicates a point where we can break a line when formatting.
 *)
let breakNull=breakWith ""

(*w
 * Pretty print a list of elements. Takes the list of elements to pretty print,
 * the function used to pretty an element and the separator to insert in
 * beetween pretty printed elements.
 *)
let join (conv:'a -> doc) (l:'a list) (sep:doc):doc=
 let first=ref true in
 List.fold_left l
  ~init:empty
  ~f:(fun beg el ->
       match (conv el),!first with
        | c,_ when c=empty -> empty
        | c,true ->
           first:=false;
           c
        | c,false ->
           beg ^^ sep ^^ c)

let sjoin (l:'a list) (sep:doc):doc=
 let first=ref true in
 List.fold_left l
  ~init:empty
  ~f:(fun beg el ->
       match el,!first with
        | c,_ when c=empty -> empty
        | c,true ->
           first:=false;
           c
        | c,false ->
           beg ^^ sep ^^ c)

open General
module P=Printf

(*w
 * ==Pretty printing style==
 *
 * Stylesheets add decoration (formatting information) to the output. They are
 * represented as classes and can be changed during runTime. The styleSheet
 * model is not threadsafe and has a very imperative feel.
 *)
type weight=
 | Normal
 | Bold
 | Underscore
 | DefaultWeight

type color=
 | Black
 | Red
 | Green
 | Yellow
 | Blue
 | Pink
 | LightBlue
 | White
 | DefaultColor

(*w
 * Stylesheets just contain informations on how color and weights to give to
 * every keyword.
 *)
type styleSheet={
 cpar:color;
 cpunct:color;
 ckwd:color;
 cbool:color;
 cident:color;
 cstring:color;
 cnumber:color;
 cop:color;
 clit:color;
 wpar:weight;
 wpunct:weight;
 wkwd:weight;
 wbool:weight;
 wident:weight;
 wstring:weight;
 wnumber:weight;
 wop:weight;
 wlit:weight;
}
let plainStyle=
 {
 cpar=DefaultColor;
 cpunct=DefaultColor;
 ckwd=DefaultColor;
 cbool=DefaultColor;
 cident=DefaultColor;
 cstring=DefaultColor;
 cnumber=DefaultColor;
 cop=DefaultColor;
 clit=DefaultColor;
 wpar=DefaultWeight;
 wpunct=DefaultWeight;
 wkwd=DefaultWeight;
 wbool=DefaultWeight;
 wident=DefaultWeight;
 wstring=DefaultWeight;
 wnumber=DefaultWeight;
 wop=DefaultWeight;
 wlit=DefaultWeight;
}

let defStyle={
 plainStyle with
  cpar=Red;
  cpunct=Red;
  ckwd=Blue;
  wkwd=Bold;
  cbool=Pink;
  cstring=Green;
  cnumber=Pink;
  cop=Red;
  clit=Yellow
}

(*w
 * ==Formatters==
 *
 * Formatters are classes we use to do the syntax-highliting part and character
 * escaping. This part is dependent on the support we're pretty printing to. We
 * are using a class, we can therefor switch formatters at run time.
 *)
class type formater=
object
 method decorate:color->weight->string->doc
 method escape:string->string
end

(*w
 * This is a very basic formatter: it doesn't add any colors nor does it escape
 * any characters. It is usefull to ouptut ascii files (source files).
 *)
let plainFormater:formater=
object
 method decorate _ _ x = text x
 method escape x = x
end

(*w
 * This formater adds escape sequences to do the syntax highlighting.
 *)
let consoleFormater:formater=
object
 val decodeWeight=function
  | Normal -> "0"
  | Bold -> "1"
  | Underscore -> "2"
  | DefaultWeight -> ""

 val decodeColor=function
  | Black -> "30"
  | Red -> "31"
  | Green -> "32"
  | Yellow -> "33"
  | Blue -> "34"
  | Pink -> "35"
  | LightBlue -> "36"
  | White -> "37"
  | DefaultColor -> ""

 method decorate color weight txt=
  let weight=decodeWeight weight
  and color=decodeColor color in
  let col=P.sprintf "\027[%s;%sm" weight color in
  (formatInst col)^^(text txt)^^(formatInst "\027[0m")

 method escape x = x
end

(*w
 * This formatter outputs to colored latex
 *)
let texFormater=
object
 val decodeWeight=function
  | Normal -> "{","}"
  | Bold -> "\\textbf{","}"
  | Underscore -> "{","}"
  | DefaultWeight -> "{","}"

 val decodeColor=function
  | Black -> "Black"
  | Red -> "Red"
  | Green -> "Green"
  | Yellow -> "Yellow"
  | Blue -> "Blue"
  | Pink -> "magenta"
  | LightBlue -> "AquaMarine"
  | White -> "White"
  | DefaultColor -> assert false

 method decorate color weight txt =
  let col=if color=DefaultColor then
   ""
  else
   P.sprintf "\\color{%s}" (decodeColor color)
  in
  match col,(decodeWeight weight) with
   | "",("{","}") -> text txt
   | _,(o,c) ->  (formatInst (o^col))^^(text txt)^^(formatInst c)

 (*w
  * This function escapes Strings to be printed as javascript strings
  *)
 method escape s=
  let b=Buffer.create (String.length s) in
  let a= Buffer.add_string b in
  let process=function
   | ' ' -> a "~"
   | '$' | '#' | '%' | '^' | '_' | '{' | '}' as c ->
      a "\\"; Buffer.add_char b c
   | '\\' -> a "\\backslash{}"
   | '\t' -> a "~~~~"
   | '\n' -> a "\\\\\n\\mbox{}"
   | c -> Buffer.add_char b c
  in
  String.iter ~f:process s;
  Buffer.contents b
end

(*w
 * This formatter outputs to colored html
 *)
let htmlFormater=
object
 val decodeWeight=function
  | Normal -> "",""
  | Bold -> "<b>","</b>"
  | Underscore -> "<u>","</u>"
  | DefaultWeight -> "",""

 val decodeColor=function
  | Black -> "black"
  | Red -> "red"
  | Green -> "green"
  | Yellow -> "yellow"
  | Blue -> "blue"
  | Pink -> "magenta"
  | LightBlue -> "lightblue"
  | White -> "white"
  | DefaultColor -> assert false

 method decorate color weight txt =
  let col=if color=DefaultColor then
   "",""
  else
   (P.sprintf "<font color=\"%s\">" (decodeColor color)),"</font>"
  in
  match col,(decodeWeight weight) with
   | ("",""),("{","}") -> text txt
   | (o1,c1),(o2,c2) ->  (formatInst (o1^o2))^^(text txt)^^(formatInst (c2^c1))

 (*w
  * This function escapes Strings to be printed as javascript strings
  *)
 method escape s=
  let b=Buffer.create (String.length s) in
  let a= Buffer.add_string b in
  let process=function
   | '<' -> a "&lt;"
   | '>' -> a "&gt;"
   | '&' -> a "&amp;"
   (*| '\n' -> a "\\\\\n\\mbox{}"*)
   | c -> Buffer.add_char b c
  in
  String.iter ~f:process s;
  Buffer.contents b
end


let autoFormater=
 (if Unix.isatty Unix.stdout then
   consoleFormater
  else
   plainFormater)

let formater=ref autoFormater

let setTexFormat ()=formater:=texFormater
let setHtmlFormat ()=formater:=htmlFormater

let ssheet=ref defStyle

let par p = !formater#decorate !ssheet.cpar !ssheet.wpar p
let punct p = !formater#decorate !ssheet.cpunct !ssheet.wpunct p
let kwd p = !formater#decorate !ssheet.ckwd !ssheet.wkwd p
let bool p = !formater#decorate !ssheet.cbool !ssheet.wbool p
let ident p = !formater#decorate !ssheet.cident !ssheet.wident p
let string p = !formater#decorate !ssheet.cstring !ssheet.wstring p
let number p = !formater#decorate !ssheet.cnumber !ssheet.wnumber p
let op p = !formater#decorate !ssheet.cop !ssheet.wop p
let lit p = !formater#decorate !ssheet.clit !ssheet.wlit p

let toString p = Pp.ppToString ~escapeFunction:(!formater#escape) 80 p

let par s =
 (par "(") ^^ s ^^ (par ")")
and brace s =
 (par "{") ^^ s ^^ (par "}")
and bracket s =
 (par "[") ^^ s ^^ (par "]")

(*w
 * This is just used to ensure we do NOT use ^^text^^ in the functions below...
 * It's an ugly yet effective hack.
 *)
let text = bottom

(*w
 * ==Monadic traversal==
 *)
module PrinterMonad=
struct
 exception NotImplemented
 type 'a m=Pp.doc
 let return _ = raise NotImplemented
 let bind _ = raise NotImplemented
end

(*w
 * A weak set of formated text
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
 * A bunch of convenience functions required by all the printers.
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
let instrSep=((punct ";")^^break)

module Convenience(S:ConvIn)=
struct
 (*w
   Prints a javascript bloc.
 *)
 let braceIndent s=
  vgrp(brace (vgrp(nest 4 (break^^s))^^break))

 let joinInstrs l=
  join S.instr l instrSep

 let bloc l =
  let r=braceIndent (joinInstrs l) in
  Ws.add blocs r;
  r

 let grpInstr i=
  if Ws.mem blocs i then
   i
  else begin
   let r=fgrp i in
   (*Ws.add blocs r;*)
   r
  end

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
  String.iter ~f:process s;
  Buffer.contents b
end
