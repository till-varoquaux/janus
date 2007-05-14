open General
gram extends AstBase {
 import expr,ident,constant,lvalue,unop,binop,program,macrobloc,macroitem;
 instr :=
 `Cpsdecl ident,[ident],instr
 | `CpsCall ident?,expr,[expr]
    (*a=b(x1...xn)*)
 | `CpsRet expr
 | `CpsTemplateCall [expr],macrobloc
 | super
}
