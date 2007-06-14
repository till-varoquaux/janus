(*w
  This is the grammar of the subset of javascript we are working with. All the
  javascript optimisation passes will map over these types.
*)
gram extends AstBase{
 import ty,constant,ident,lvalue,unop,binop,program,macrobloc,expr,instr,macroitem;
 instr:=
| `Fundecl ident,[ident],instr
| `Labeled ident,instr
| `Break ident
| `Continue ident
    (*w
      Thi is used to push an object on the stack, the ^^ident list^^ is a
      signature of the object we shall push.

      This signature is used by the optimisation passes, faillure to provide a
      correct signature will result in poossibly corrupted output
    *)
| `WithCtx expr,instr,[ident]
   (*w
     This is the empty return statement.

     TODO: we should find a way to unify it with `Ret. This probably requires
     polymorphic variant difference.
   *)
| `Return
| `Var ident
| super
}
