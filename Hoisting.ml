(*w
  This javascript to javascript optimsation pass performs hoisting of function
  definitions and varibale declarations. It relies on the facts that variables
  have a unique name and that javascript uses late bindings.

  All the function definitions are collected and moved to the top of there
  containing scope (with statements or functions). since variable names are
  unique (and javascript has no block scoping) vars can be safelly moved to the
  top of there containing function.
*)
open General
open ScopeInfo

(*w The null instruction: doesn't do anything*)
let null=`Bloc []
module Mon=Monad.StateMonad(
 struct
  type t=AstJs.instr list
 end
)

module T=AstJs.Trav.Map(Mon)

module Hoist=T.Make(
 functor(S:T.T) ->
 struct
  module Super=T.Base(S)
  include Super

  let program p funs=
   let prog,funs = Super.program p funs
   in
   (funs@prog),funs

  let instr i funs=
   match i with
    | `Fundecl (name,args,i) ->
       let hInstr,hFuns=instr i [] in
       let vars=ref [] in
       StringSet.iter (fun i -> vars:=(`Var i)::!vars) (ScopeInfo.instr i).defined;
       null,
       (`Fundecl (name,args,`Bloc ((!vars)@hFuns@[hInstr])))::funs
    | `WithCtx (e,i,si) ->
       (*w
         the only purpose of using "with" is to ensure closures are correctly
         defined, therefor we don't want to hoist function declarations any higher...
        *)
       let capturesLocals f=
        let captured = (ScopeInfo.instr f).captured in
        List.exists (fun v -> StringSet.mem v captured) si
       in
       let hInstr,hFuns=instr i [] in
       let capFuns,hoistFuns = List.partition capturesLocals hFuns in
       `WithCtx (e,`Bloc(capFuns@[hInstr]),si),
       hoistFuns@funs
    | `Var _ ->
       (*w
         We can safely drop the var def here: they will be hoisted and
         recalculated from scopeInfo.
       *)
       null,funs
    |_ -> Super.instr i funs
 end)

let pass:#Optimise.pass=
object
 method run p=Mon.run (Hoist.program p) []
 method name="hoisting"
 method description="hoisting of function and var declarations"
end
