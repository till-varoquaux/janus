(*w
  This is a very dump pass: it identifies fundecls since they don't exist in the
  language we are compiling from.
*)

module T=AstJs.Trav.Map(Monad.Id)

module D=T.Make(
 functor(S:T.T) ->
 struct
  module Super=T.Base(S)
  include Super

  let rec bloc =function
   | [] -> []
   | `Var name :: `Assign((`Ident id),`Fun(args,i))::l when id=name ->
      `Fundecl(id,args,(instr i))::(bloc l)
   | i::l ->
      (instr i)::(bloc l)
  and instr = function
   | `Bloc b -> `Bloc (bloc b)
   | i -> Super.instr i
  let program = bloc
 end
)

let pass:#Optimise.pass=
object
 method run = D.program
 method name="fundecl"
 method description="function declarations squishing"
end
