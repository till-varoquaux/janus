gram{
 ident := string;
 constant:= `Bool bool | `Int int | `Float float;
 expr := `Cst constant | `Lval lvalue | `Call expr,[expr]
   | `Unop unop,expr | `Binop binop,expr,expr;
 unop:= `Not | `Minus;
 lvalue :=`Ident ident | `Array lvalue,expr;
 binop := `Eq | `Neq | `Lt | `Le | `Gt | `Ge
   | `Add | `Sub | `Mul | `Div | `Mod | `And | `Or;
 instr :=
  `Vdecl [ident]
 | `Call expr,[expr]
 | `Vardecl ident,expr
 | `Fundecl ident,[ident],instr
 | `Assign lvalue,expr
 | `If expr,instr,instr
 | `While expr,instr
 | `TemplateCall [expr],macrobloc
 | `Bloc [instr]
 | `Ret expr;
 macroitem:=`Ident int (*De bruijn index*) | `Literal string;
 macrobloc:=[macroitem];
 program := [instr]
}
