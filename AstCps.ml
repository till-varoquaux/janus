gram extends AstBase {
 import ty,constant,ident,lvalue,unop,binop,program,macrobloc;
 ty :=
| `T (*Base type*)
| `Arrow [ty],ty (*Normal application*)
| `CpsArrow [ty],ty (*Cps function*);
 macroitem:= `Ident ident | `Literal string;
 instr := `CpsMacro ident,[ident],macrobloc,[ty]
| `Macro ident,[ident],macrobloc,[ty]
| `Var ident,expr
| super;
 expr :=  `Fun [ident],instr | `Typed expr,ty | super
}
