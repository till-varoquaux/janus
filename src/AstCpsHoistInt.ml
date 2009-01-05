grammar extends AstCpsInt(ty;constant;ident;lvalue;unop;binop;program;instr) =
begin
 expr:=
 | `Hoist (expr,instr)
   (*The marked instruction should be hoisted before the one holding this
     expression*)
 | super
end
