gram{
 ident := string;
 constant:= `Bool bool | `Int int | `Float float | `String string;
 expr := `Cst constant | `Lval lvalue | `Call expr,[expr]
   | `Unop unop,expr | `Binop binop,expr,expr;
 unop:= `Not | `Minus;
 (*TODO: Access and Array should be moved to expr for more
   flexibility. Currently we cannot write f()[3] for instance*)
 lvalue :=`Ident ident | `Array lvalue,expr | `Access lvalue,ident;
 binop := `Eq | `Neq | `Lt | `Le | `Gt | `Ge
   | `Add | `Sub | `Mul | `Div | `Mod | `And | `Or;
 instr :=
  `Call expr,[expr]
 | `Fundecl ident,[ident],instr
 | `Assign lvalue,expr
 | `If expr,instr,instr
 | `While expr,instr
 | `TemplateCall [expr],macrobloc
 | `Bloc [instr]
 | `Ret expr;
 ty;
 macroitem:=`Ident int (*De bruijn index*) | `Literal string;
 macrobloc:=[macroitem];
 program := [instr]
}
