gram extends AstCpsInt{
 import ty,constant,ident,lvalue,unop,binop,program,macrobloc,expr,macroitem;
 instr:=
| `Cps instr
   (*The marked instruction should be cps converted*)
| super
}
