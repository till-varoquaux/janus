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
| `CpsCall ident?,expr,[expr] (*TODO check wether we could extend this to lvalues*)
| `CallCC ident?,expr,[expr]
| `Throw expr,expr
| `Abort
| super
}
