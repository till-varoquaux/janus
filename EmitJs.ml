(*w
  This is where we will print a javascript document. We will emit a AstJs tree.
*)
open Printf
open Pp
open AstJs
open General

let breakNull=breakWith ""
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

(*w
  Pretty printing style
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

open Style

(*This is just used to unsure we do NOT use ^^text^^ in the functions below...*)
type abstract
let text : abstract = Obj.magic ()

let rec ident i=
 Style.ident i

and constant = function
 | `Bool true -> bool "true"
 | `Bool false -> bool "false"
 | `Int i -> number (string_of_int i)
 | `Float f -> number (string_of_float f)
 | `String s ->  Style.string (sprintf "\"%s\"" (escape s))

(*TODO: replace with jerom's function to place a minimal amount of parenthesis*)
and expr ?(guard=false) =
 let wrap b=
  if guard then
   (par "(") ^^ b ^^ (par ")")
  else
   b
 and ep e=
  expr ~guard:true e
 in
 function
  | `Fun(args,b) ->
     let b=(match b with
             | `Bloc _ -> b
             | i -> `Bloc [i]) in
     wrap ((kwd "function"^^(par "("))^^
            (join ident args (punct ","))^^ (par ")") ^^ (instr b))
  | `Cst c -> constant c
  | `Ident i -> ident i
  | `Call (f,args) ->
     let args=join expr args (punct ",")
     in
     (ep f) ^^ (par "(")^^args^^ (par ")")
  | `Array (elems) ->
     let elems=join expr elems (punct ",")
     in
     (par "[")^^elems^^(par "]")
  | `Unop (op,e) ->
     wrap ((unop op) ^^ (ep e))
  | `Binop (op,e1,e2) ->
     wrap (ep e1) ^^ (binop op) ^^ (ep e2)
  | `ArrayAccess (e,idx) ->
     (expr e)^^(par "[")^^(expr idx) ^^ (par "]")
  | `Obj(pl) ->
     agrp(nest 4 ((par "{")^^ breakNull
                  ^^(join
                      (fun (i,e) -> (ident i) ^^ (punct ":") ^^ (expr e))
                      pl
                      ((punct ",") ^^ breakNull)
                    )
                  ^^breakNull^^(par "}")))
  | `ObjAccess (e,i) -> (expr e) ^^ (punct ".") ^^ (ident i)

and cond e=
 (par "(") ^^ (expr e) ^^ (par ")")

and unop u = op (match u with
                    | `Not -> "!"
                    | `Minus -> "-"
                  )

and binop b = op (match b with
                     | `Eq -> "=="
                     | `Neq -> "!="
                     | `Lt -> "<"
                     | `Le -> "<="
                     | `Gt -> ">"
                     | `Ge -> ">="
                     | `Add -> "+"
                     | `Sub -> "-"
                     | `Mul -> "*"
                     | `Div -> "/"
                     | `Mod -> "%"
                     | `And -> "&&"
                     | `Or  -> "||")

and lvalue = function
 | `Ident i -> ident i
 | `ArrayAccess (l,e) -> (lvalue l) ^^ (par "[") ^^ (expr e) ^^ (par "]")
 | `ObjAccess (l,i) -> (lvalue l) ^^ (punct ".") ^^ (ident i)

and macroelem al= function
 | `Literal l ->  lit l
 | `Ident i -> List.nth al i

and instr (i:instr)=
 let r=match i with
  | `WithCtx (e,b,_) -> (kwd "with")^^(par "(") ^^ (expr e ) ^^ (par ")") ^^ (blocOrInstr b)
  | `Bloc il -> bloc il
  | `Var i -> (kwd "var ") ^^ (ident i)
     (*TODO: collapse `Var lists*)
     (*TODO: Collapse `Var and `Assign*)
  | `Fundecl(i,args,b) ->
     let b=(match b with
             | `Bloc _ -> b
             | i -> `Bloc [i]) in
     (kwd "function ")^^(ident i)^^(par "(")^^
      (join ident args (punct ","))^^ (par ")") ^^ (instr b)
  | `Assign (l,e) -> (lvalue l) ^^ (punct "=") ^^ (expr e)
  | `If (e,b,`Bloc []) -> (kwd "if") ^^ (cond e) ^^ (blocOrInstr b)
  | `If (e,b1,(`If _ as b2)) ->
     (kwd "if") ^^ (cond e) ^^ (blocOrInstr ~breakAfter:true b1) ^^
      (kwd "else ")^^(instr b2)
  | `If (e,b1,b2) ->(kwd "if") ^^ (cond e) ^^ (blocOrInstr ~breakAfter:true b1)
     ^^ (kwd "else") ^^ (blocOrInstr b2)
  | `While (e,b) -> (kwd "while") ^^ (cond e) ^^ (blocOrInstr b)
  | `Labeled (lbl,i) -> (ident lbl) ^^(punct ":")^^(instr i)
  | `Continue lbl -> (kwd "continue") ^^ break ^^ (ident lbl)
  | `Break lbl -> (kwd "break") ^^ break ^^ (ident lbl)
  | `Call (f,args) ->
     let args=join expr args (punct ",")
     in
     (expr f) ^^ (par "(")^^args^^ (par ")")
  | `Ret (e) ->
     (kwd "return")^^break^^(expr e)
  | `Return -> kwd "return"
  | `TemplateCall (al,b) ->
     let al=List.map expr al in
     join (macroelem al) b empty
 in
 if r=empty then
  empty
 else
  fgrp r

and aff (i,e)=
  (ident i) ^^ (punct "=") ^^ (expr e)

and join_instrs l=
 join instr l ((punct ";")^^break)

and printBloc = function
 | `Bloc l -> bloc l
 | i -> bloc [i]

and blocOrInstr ?(breakAfter=false) = function
 | `Bloc b -> bloc b
 | i ->
    if breakAfter then
     vgrp(nest 4 (break^^(instr i))^^ break)
    else
     vgrp(nest 4 (break^^(instr i)))

and bloc l =
 vgrp((par "{")^^(vgrp(nest 4 (break^^(join_instrs l)))^^break)^^(par "}"))

and print (p:program):unit=
 print_string (ppToString 80 (vgrp((join_instrs p)^^break)))
