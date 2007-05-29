gram extends AstBase{
 import ty,constant,ident,lvalue,unop,binop,program,macrobloc,expr,instr,macroitem;
 instr:=
  `Loop ident,instr
| `Continue ident
| `WithCtx expr,instr
| `Return
| `Var ident
| super
}
