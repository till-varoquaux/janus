gram{
 ident := string;
 constant:= `Bool bool | `Int int | `Float float | `String string;
 expr :=
| `Fun [ident],instr
| `Cst constant
| `Ident ident
| `Call expr,[expr]
| `Unop unop,expr
| `Binop binop,expr,expr
| `Array [expr]
| `Obj [(ident,expr)]
| `ArrayAccess expr,expr
| `ObjAccess expr,ident;
 unop:= `Not | `Minus;
 lvalue :=`Ident ident | `ArrayAccess lvalue,expr | `ObjAccess lvalue,ident;
 binop := `Eq | `Neq | `Lt | `Le | `Gt | `Ge
| `Add | `Sub | `Mul | `Div | `Mod | `And | `Or;
 instr :=
| `Call expr,[expr]
| `Assign lvalue,expr
| `If expr,instr,instr
| `While expr,instr
| `TemplateCall [expr],macrobloc
| `Bloc [instr]
| `Ret expr
| `Expr expr
| `Var ident,expr?;
 ty;
 macroitem:=`Ident int (*De bruijn index*) | `Literal string;
 macrobloc:=[macroitem];
 program := [instr]
}
