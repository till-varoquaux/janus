open General
gram extends AstBase {
 import expr,ident,constant,lvalue,unop,binop,program,macrobloc,macroitem;
 (*We do the upcast here since it is painfull to do in AstCpsInt -> AstJs pass*)
 expr :=
 `EmptyCtx
| super;
 instr :=
  `Cps instr
| `CpsCall ident?,expr,[expr]
   (**
      the ident is the return value ("a" in "a=b(x1...xn)"
   *)
| `Var ident,expr
| super
}
