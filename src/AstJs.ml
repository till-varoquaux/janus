(*w
 * ====The javascript grammar====
 * This is the grammar of the subset of javascript we are working with. All the
 * javascript optimisation passes will map over these types.
 *)
grammar extends AstBase(ty;constant;ident;lvalue;unop;binop;program;expr) =
begin
 instr:=
  | `Fundecl (ident,[ident],instr)
  | `Labeled (ident,instr)
  | `Break ident?
  | `Continue ident?
    (*w
     * This is used to push an object on the stack, the ^^ident list^^ is a
     * signature of the object we shall push.
     *
     * This signature is used by the optimisation passes, faillure to provide a
     * correct signature will result in possibly corrupted output
     *)
  | `WithCtx (expr,instr,[ident])
  | super
end
