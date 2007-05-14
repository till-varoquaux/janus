gram extends AstBase{
 import constant,ident,lvalue,unop,binop,program,macrobloc,expr,instr,macroitem;
 instr:=
  `Loop ident,instr
| `Continue ident
| `Return
| super
}
