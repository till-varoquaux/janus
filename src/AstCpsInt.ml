grammar extends AstBase(ty;ident;constant;lvalue;unop;binop;program) = begin
 expr:=
  | `CpsFun ([ident],instr)
  | super;
 instr :=
   (*w
     the ident is the return value ("a" in "a=b(x1...xn)"
   *)
  | `CpsCall (ident?,expr,[expr])
  | `CpsRet expr?
  | `CallCC (ident?,expr,[expr])
  | `Throw (expr,expr)
  | super
end
