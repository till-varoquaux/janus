gram extends AstBase {
 import ty,constant,ident,lvalue,unop,binop,program,macrobloc;
 ty :=
| `T (*Base type*)
| `Arrow [ty],ty (*Normal application*)
| `CpsArrow [ty],ty (*Cps function*);
 macroitem:= `Ident ident | `Literal string;
 instr :=
| `CallCC expr
| `Throw expr,expr
| `CpsMacro ident,[ident],macrobloc,[ty]
| `Macro ident,[ident],macrobloc,[ty]
| `Var ident,expr
| super;
 expr := `Typed expr,ty | super
}
