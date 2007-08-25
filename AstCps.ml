gram extends AstBase {
 import ty,constant,ident,lvalue,unop,binop,program;
 ty :=
| `T (*Base type*)
| `Arrow [ty],ty (*Normal application*)
| `CpsArrow [ty],ty (*Cps function*);
 instr :=
| `CallCC expr,[expr]
| `BlockingEv expr,[expr]
| `Throw expr,expr
| `Abort
| super;
 expr :=
| `CallCC expr,[expr]
| `BlockingEv expr,[expr]
| `Typed expr,ty
| `Hoist expr,instr
| super
}
