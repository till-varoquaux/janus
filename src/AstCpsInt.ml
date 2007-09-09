gram extends AstBase {
 import ty,ident,constant,lvalue,unop,binop,program;
 expr:=
| `CpsFun [ident],instr
| super;
 instr :=
   (*w
     the ident is the return value ("a" in "a=b(x1...xn)"
   *)
| `CpsCall ident?,expr,[expr]
| `CpsRet expr?
| `CallCC ident?,expr,[expr]
| `Throw expr,expr
| super
}
