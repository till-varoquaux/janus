gram extends AstCpsInt{
 import ty,constant,ident,lvalue,unop,binop,program,expr;
 instr:=
| `Cps instr
   (*The marked instruction should be cps converted*)
| super
}
