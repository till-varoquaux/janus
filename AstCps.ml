gram extends AstBase {
 import ty,constant,ident,lvalue,unop,binop,program,macrobloc;
 ty :=
| `T (*Base type*)
| `Arrow [ty],ty (*Normal application*)
| `CpsArrow [ty],ty (*Cps function*);
 macroitem:= `Ident ident | `Literal string;
 instr :=
| `CallCC expr,[expr]
| `Throw expr,expr
| `CpsMacro ident,[ident],macrobloc,[ty]
| `Macro ident,[ident],macrobloc,[ty]
| `Abort
| super;
 expr :=
| `CallCC expr,[expr]
| `Typed expr,ty
| `Hoist expr,instr
| super
}
