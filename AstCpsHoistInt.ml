gram extends AstCpsInt{
 import ty,constant,ident,lvalue,unop,binop,program,instr;
 expr:=
| `Hoist expr,instr
   (*The marked instruction should be hoisted before the one holding this
     expression*)
| super
}
