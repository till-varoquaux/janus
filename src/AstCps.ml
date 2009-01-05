gram extends AstBase {
  (* TODO delete ty *)
 import ty,constant,ident,lvalue,unop,binop,program;
 ty :=
| `T (*Base type*)
| `Arrow [ty],ty (*Normal application*)
| `CpsArrow [ty],ty (*Cps function*);
 instr :=
| `CallCC expr,[expr]
| `BlockingEv expr,[expr]
| `Throw expr,expr
| super;
 expr :=
| `CallCC expr,[expr]
| `BlockingEv expr,[expr]
| `Typed expr,ty
| `Hoist expr,instr
| super
}
