(*w
 * ====AstBase pretty printer====
 * This is the extensible printer for [[../AstBase.ml.html|AstBase]]'s tree. It
 * makes heavy usage of open reccursion to allow extensibility.
 *)
open Printer

module Process(From:AstBase.Trav.T)=
struct
 module Conv=AstBase.Trav.Conv(From)(From)(PrinterMonad)
 module In=Conv.In
 module Main(Self:Conv.Translation):Conv.PartialTranslation=
 struct
  include Convenience(struct module In=From include Self end)

  let ident i = Printer.ident i
  let ty _ = assert false
  let constant = function
   | `Bool true -> bool "true"
   | `Bool false -> bool "false"
   | `Int i -> number (string_of_int i)
   | `Float f -> number (string_of_float f)
   | `String s ->  string (P.sprintf "\"%s\"" (escape s))

  (*w
   * We need to match subexpressions to place our parenthesis (well this is not
   * exactly true... we could place parenthesis around all subexpressions). The
   * simple algorithm needs to have a boolean returned by some subexpressions.
   *
   * We are faced here with an issue: we want just this function to return
   * additional informations. We are faced here with a dilemna on how to return
   * that boolean information, we could:
   *
   * - Use a monad that returns a couple to get this boolean.
   * - Use a global reference to encode this.
   * - Extend the return type to contain that additional information (this
   * would, however make this type a lot less meaningfull)
   *
   * We will use a weak set to keep these expressions, this is very simillar to
   * references but not unsafe.
   *)
  let fragileSet=Ws.create 17
  let fragile p=
   Ws.add fragileSet p;
   p
  let ep e=
   let e'=Self.expr e in
   if Ws.mem fragileSet e' then begin
    par e'
   end else begin
    e'
   end

  let expr =
   function
    | `Fun(args,b) ->
       let b=protectInstr b in
       fragile
        (kwd "function"^^
          (par (mapConcat args ~f:Self.ident ~sep:(punct ",")))^^
          b)
    | `Cst c -> Self.constant c
    | `Ident i -> Self.ident i
    | `Call (f,args) ->
       let args=mapConcat args ~f:Self.expr ~sep:(punct ",")
       in
       (ep f) ^^ (par args)
    | `Array (elems) ->
       let elems=mapConcat elems ~f:Self.expr ~sep:(punct ",")
       in
       (bracket elems)
    | `Unop (op,e) ->
       fragile ((Self.unop op) ^^ (ep e))
    | `Binop (op,e1,e2) ->
       fragile ((ep e1) ^^ (Self.binop op) ^^ (ep e2))
    | `ArrayAccess (e,idx) ->
       (Self.expr e)^^(bracket (Self.expr idx))
    | `Obj(pl) ->
       agrp(nest 4 (brace (breakNull
                           ^^(mapConcat pl
                               ~sep:((punct ",") ^^ breakNull)
                               ~f:begin fun (i,e) ->
                                (Self.ident i) ^^ (punct ":") ^^ (Self.expr e)
                               end
                             )
                           ^^breakNull)))
    | `ObjAccess (e,i) -> (Self.expr e) ^^ (punct ".") ^^ (Self.ident i)


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
   | `Ident i -> Self.ident i
   | `ArrayAccess (l,e) -> (Self.lvalue l) ^^ (bracket (Self.expr e))
   | `ObjAccess (l,i) -> (Self.lvalue l) ^^ (punct ".") ^^ (Self.ident i)

  let instr (i)=
   let r=match i with
    | `WithCtx (e,b,_) -> (kwd "with")^^(par (Self.expr e )) ^^ (blocOrInstr b)
    | `Bloc il -> bloc il
    | `Fundecl(i,args,b) ->
       let b=protectInstr b in
       (kwd "function")++
        (ident i)^^
        (par(mapConcat args ~f:ident ~sep:(punct ",")))^^b
    | `Assign (l,e) -> (Self.lvalue l) ^^ (punct "=") ^^ (Self.expr e)
    | `Var (i,None) -> (kwd "var") ++ (Self.ident i)
      (*TODO: collapse empty `Var lists*)
    | `Var (i,Some e) ->
       (kwd "var") ++ (Self.ident i) ^^ (punct "=") ^^ (Self.expr e)
    | `If (e,b1,b2) ->
       (kwd "if") ^^ (cond e) ^^
        (blocOrInstr ~breakAfter:true b1)^^
        (kwd "else") ^^
        (blocOrInstr b2)
    | `While (e,b) -> (kwd "while") ^^ (cond e) ^^ (blocOrInstr b)
    | `Call (f,args) ->
       let args=mapConcat args ~sep:(punct ",") ~f:Self.expr
       and e'=Self.expr f in
       let f'=
        if Ws.mem fragileSet e' then begin
         par e'
        end else begin
         e'
        end in
       f' ^^ (par args)
    | `Expr e -> Self.expr e
    | `Ret (Some e) ->
       (kwd "return")^^break^^(ep e)
    | `Ret None -> kwd "return"
   in
   grpInstr r

  let program p = vgrp((joinInstrs p)^^break)
 end
end
