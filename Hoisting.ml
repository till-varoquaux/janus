module Env=
 struct
  module StringSet=Set.Make(String)
  type t={
   vars:StringSet.t;
   funcs:AstJs.instr list
  }
  let var v e=
   {e with
    vars=StringSet.add v e.vars
   }

  let getVars s =
   StringSet.elements s.vars

  let func v e=
   {e with
    funcs=v::e.funcs
   }

  let getFuncs e =
   e.funcs

  let resetFuncs e=
   {e with
     funcs=[]
   }

  let merge e1 e2=
   {
    vars=StringSet.union e1.vars e2.vars;
    funcs=e1.funcs@e2.funcs
   }

  let empty=
   {
    vars=StringSet.empty;
    funcs=[]
   }
 end
module Mon=Monad.StateMonad(Env)

module T=AstJs.Trav.Map(Mon)

module Hoist=T.Make(
 functor(S:T.T) ->
 struct
  module Super=T.Base(S)
  include Super
  let null=`Bloc [](*The null instruction: doesn't do anything*)
  (**
      Outputs all the declarations from a given environement
   *)
  let makeHeader s=
   (Env.getFuncs s)
   @
   (List.map (fun i -> `Var i) (Env.getVars s))

  let program p env=
   let prog,env = Super.program p env
   in
   ((Env.getFuncs env)@prog),env

  let instr i env=
   match i with
    | `Fundecl (name,args,i) ->
       let hInstr,hEnv=instr i Env.empty in
       null,
       (Env.func (`Fundecl (name,args,`Bloc ((makeHeader hEnv)@[hInstr]))) env)
    | `WithCtx (e,i) ->
       let hInstr,hEnv=instr i Env.empty in
       `WithCtx (e,`Bloc(Env.getFuncs hEnv@[hInstr])),
       Env.merge env (Env.resetFuncs hEnv)
    | `Var i -> null,(Env.var i env)
    | _ -> Super.instr i env
 end)

let pass:#Optimise.pass=
object
 method run p=Mon.run (Hoist.program p) Env.empty
 method name="hoisting"
 method description="hoisting of function and var declarations"
end
