(*w
  This is where we will print a javascript document. We will emit a AstJs tree.
*)
open Printf
open Pp
open AstJs

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

let rec ident id=
 text id
and constant = function
 | `Bool true -> text "true"
 | `Bool false -> text "false"
 | `Int i -> text (string_of_int i)
 | `Float f -> text (string_of_float f)
 | `String s -> text (sprintf "\"%s\"" (escape s))

(*TODO: replace with jerom's function*)
and expr ?(guard=false) =
 let wrap b=
  if guard then
   (text "(") ^^ b ^^ (text ")")
  else
   b
 and ep e=
  expr ~guard:true e
 in
 function
  | `EmptyCtx -> text "new Object()"
  | `Cst c -> constant c
  | `Lval v -> lvalue v
  | `Call (f,args) ->
     let args=join expr args (text ",")
     in
     (expr f) ^^ (text "(")^^args^^ (text ")")
  | `Unop (op,e) ->
     wrap ((unop op) ^^ (ep e))
  | `Binop (op,e1,e2) ->
     wrap (ep e1) ^^ (binop op) ^^ (ep e2)
  (*| `Fun (args,b) -> (text "function(") ^^ (join ident args (text ","))
     ^^ (text ")") ^^ (bloc b)*)

and cond e=
 (text "(") ^^ (expr e) ^^ (text ")")

and unop u = text (match u with
                    | `Not -> "!"
                    | `Minus -> "-"
                  )

and binop b = text (match b with
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
 | `Array (l,e) -> (lvalue l) ^^ (text "[") ^^ (expr e) ^^ (text "]")
 | `Access (l,i) -> (lvalue l) ^^ (text ".") ^^ (ident i)

and macroelem al= function
 | `Literal l -> text l
 | `Ident i -> List.nth al i

and instr (i:instr)=
 let r=match i with
  | `WithCtx (e,b) -> (text "with(") ^^ (expr e ) ^^ (text ")") ^^ (blocOrInstr b)
  | `Bloc il -> bloc il
  | `Var i -> (text "var ") ^^ (ident i)
     (*TODO: collapse `Var lists*)
     (*TODO: Collapse `Var and `Assign*)
  | `Fundecl(i,args,b) ->
     let b=(match b with
             | `Bloc _ -> b
             | i -> `Bloc [i]) in
     (text "function ")^^(ident i)^^(text "(")^^
      (join ident args (text ","))^^ (text ")") ^^ (instr b)
  | `Assign (l,e) -> (lvalue l) ^^ (text "=") ^^ (expr e)
  | `If (e,b,`Bloc []) -> (text "if") ^^ (cond e) ^^ (blocOrInstr b)
  | `If (e,b1,(`If _ as b2)) ->
     (text "if") ^^ (cond e) ^^ (blocOrInstr ~breakAfter:true b1) ^^
      (text "else ")^^(instr b2)
  | `If (e,b1,b2) ->(text "if") ^^ (cond e) ^^ (blocOrInstr ~breakAfter:true b1)
     ^^ (text "else") ^^ (blocOrInstr b2)
  | `While (e,b) -> (text "while") ^^ (cond e) ^^ (blocOrInstr b)
  | `Loop (lbl,b) -> (ident lbl) ^^ (text ":while(true)") ^^ (blocOrInstr b)
  | `Continue lbl -> (text "continue") ^^ break ^^ (ident lbl)
  | `Call (f,args) ->
     let args=join expr args (text ",")
     in
     (expr f) ^^ (text "(")^^args^^ (text ")")
  | `Ret (e) ->
     (text "return")^^break^^(expr e)
  | `Return -> text "return"
  | `TemplateCall (al,b) ->
     let al=List.map expr al in
     join (macroelem al) b empty
 in
 if r=empty then
  empty
 else
  fgrp r

and aff (i,e)=
  (ident i) ^^ (text "=") ^^ (expr e)

and join_instrs l=
 join instr l ((text ";")^^break)

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
 vgrp((text "{")^^(vgrp(nest 4 (break^^(join_instrs l)))^^break)^^(text "}"))

and print (p:program):unit=
 print_string (ppToString 80 (vgrp((join_instrs p)^^break)))
