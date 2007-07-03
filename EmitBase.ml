(*w
  ====AstBase pretty printer====
  This is the extensible printer for [[AstBase.ml.html|AstBase]]'s tree. It makes
  heavy usage of open reccucrsion to allow etensibility.
*)
open Printer

module Process(From:AstBase.Trav.AstDef)=
struct
 module Conv=AstBase.Trav.Conv(From)(From)(PrinterMonad)

 module Main(S:Conv.Translation):Conv.PartialTranslation=
 struct

  module In=S.In
  module Out=S.Out

  include Convenience(S)

  let ident i =
   Style.ident i
  let ty _ = assert false
  let constant = function
   | `Bool true -> bool "true"
   | `Bool false -> bool "false"
   | `Int i -> number (string_of_int i)
   | `Float f -> number (string_of_float f)
   | `String s ->  Style.string (sprintf "\"%s\"" (escape s))

  let expr =
   (*w
     We need to match subexpressions to place our parenthesis (well this is not
     exactly true... we could place parenthesis around all subexpressions). The
     simple algorithm needs to have a boolean returned by some subexpressions.

     We are faced here with an issue: we want just this function to return
     additional informations. We are faced here with a dilemna on how to return
     that boolean information, we could:

     - Use a monad that returns a couple to get this boolean.
     - Use a global reference to encode this.
     - Extend the return type to contain that additional information (this
     would, however make this type a lot less meaningfull)

     We will use a weak set to keep these expressions, this is very simillar to
     references but not unsafe.
   *)
   let fragileSet=Ws.create 17 in
   let fragile p=
    Ws.add fragileSet p;
    p
   and ep e=
    let e'=S.expr e in
    if Ws.mem fragileSet e' then
     par e'
    else
     e'
   in
   function
    | `Fun(args,b) ->
       let b=protectInstr b in
       fragile
        (kwd "function"^^(par (join S.ident args (punct ","))) ^^ b)
    | `Cst c -> S.constant c
    | `Ident i -> S.ident i
    | `Call (f,args) ->
       let args=join S.expr args (punct ",")
       in
       (ep f) ^^ (par args)
    | `Array (elems) ->
       let elems=join S.expr elems (punct ",")
       in
       (bracket elems)
    | `Unop (op,e) ->
       fragile ((S.unop op) ^^ (ep e))
    | `Binop (op,e1,e2) ->
       fragile ((ep e1) ^^ (S.binop op) ^^ (ep e2))
    | `ArrayAccess (e,idx) ->
       (S.expr e)^^(bracket (S.expr idx))
    | `Obj(pl) ->
       agrp(nest 4 (brace (breakNull
                           ^^(join begin fun (i,e) ->
                               (S.ident i) ^^ (punct ":") ^^ (S.expr e)
                              end
                               pl
                               ((punct ",") ^^ breakNull)
                             )
                           ^^breakNull)))
    | `ObjAccess (e,i) -> (S.expr e) ^^ (punct ".") ^^ (S.ident i)


  let unop u = op (match u with
                    | `Not -> "!"
                    | `Minus -> "-"
                  )

  let binop b = op (match b with
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

  let lvalue = function
   | `Ident i -> S.ident i
   | `ArrayAccess (l,e) -> (S.lvalue l) ^^ (bracket (S.expr e))
   | `ObjAccess (l,i) -> (S.lvalue l) ^^ (punct ".") ^^ (S.ident i)

  let macroitem _= assert false
  let macrobloc _= assert false

  let instr (i)=
   (*w
     Wether we will need to group the result in a fgrp
   *)
   let grp=ref true in
   let r=match i with
    | `WithCtx (e,b,_) -> (kwd "with")^^(par (S.expr e )) ^^ (blocOrInstr b)
    | `Bloc il -> grp:=false;bloc il
    | `Var i -> (kwd "var ") ^^ (ident i)
       (*TODO: collapse `Var lists*)
       (*TODO: Collapse `Var and `Assign*)
    | `Fundecl(i,args,b) ->
       let b=protectInstr b in
       (kwd "function ")^^(ident i)^^(par(join ident args (punct ",")))^^b
    | `Assign (l,e) -> (S.lvalue l) ^^ (punct "=") ^^ (S.expr e)
    | `If (e,b1,b2) ->(kwd "if") ^^ (cond e) ^^ (blocOrInstr ~breakAfter:true b1)
       ^^ (kwd "else") ^^ (blocOrInstr b2)
    | `While (e,b) -> (kwd "while") ^^ (cond e) ^^ (blocOrInstr b)
    | `Call (f,args) ->
       let args=join S.expr args (punct ",")
       in
       (S.expr f) ^^ (par args)
    | `Expr e -> S.expr e
    | `Ret (e) ->
       (kwd "return")^^break^^(S.expr e)
        (*w This is to hard to compile without breaking abstraction, we'll let the
          specialized functions handle it*)
    | `TemplateCall _ -> assert false
   in
   if !grp then
    fgrp r
   else
    r

  let program p=
   vgrp((joinInstrs p)^^break)
 end
end
