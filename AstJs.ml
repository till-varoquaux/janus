gram extends AstBase{
 import constant,ident,lvalue,unop,binop,program,macrobloc,expr,instr,macroitem;
 expr:=
  `EmptyCtx
| super;
 instr:=
  `Loop ident,instr
| `Continue ident
| `WithCtx expr,instr
| `Return
| super
}
