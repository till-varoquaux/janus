open General
gram extends AstBase {
 import expr,ident,constant,lvalue,unop,binop,program,macrobloc,macroitem;
 (*We do the upcast here since it is painfull to do in AstCpsInt -> AstJs pass*)
 expr :=
 `EmptyCtx
| super;
 instr :=
 `Cpsdecl ident,[ident],instr
 | `CpsCall ident?,expr,[expr]
    (*a=b(x1...xn)*)
 | `CpsRet expr
 | `CpsTemplateCall [expr],macrobloc
 | super
}
