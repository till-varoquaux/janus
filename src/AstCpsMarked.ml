grammar extends AstCpsInt(ty;constant;ident;lvalue;unop;binop;program;expr) =
begin
 instr:=
  | `Cps instr
   (*The marked instruction should be cps converted*)
  | super
end
