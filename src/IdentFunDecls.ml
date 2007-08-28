(*w
 * ====Identify named function declarations====
 * This is a very dumb pass: it identifies fundecls since they don't exist in the
 * language we are compiling from.
 *
 * This function relies on the fact that local vars are declared only once.
 *)

module T=AstJs.Trav.Map(Monad.Id)

module D=T.Make(
 functor(S:T.Translation) ->
 struct
  module Super=T.Base(S)
  include Super

  let instr = function
   | `Var (name,Some (`Fun(args,i))) -> `Fundecl(name,args,(instr i))
   | i -> Super.instr i
 end
)

let pass:#Optimise.pass=
object
 method run = D.program
 method name="fundecl"
 method description="function declarations squishing"
end
