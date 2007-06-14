type baseTy=AstCps.ty

gram extends AstBase {
 import ty,expr,ident,constant,lvalue,unop,binop,program,macrobloc,macroitem;
 expr:=
| `CpsFun [ident],instr
| super;
 ty :=
| baseTy
| `Macro macrobloc,[baseTy]
| `CpsMacro macrobloc,[baseTy];
 instr :=
| `Cps instr
   (*w
     the ident is the return value ("a" in "a=b(x1...xn)"
   *)
| `CpsCall ident?,expr,[expr]
| `CallCC ident?,expr
| `Throw expr,expr
| `Var ident,expr
| super
}
