(*w
 * ====Hoisting====
 * This javascript to javascript optimisation pass performs hoisting of
 * function definitions and variable declarations. It relies on the facts that
 * variables have a unique name and that javascript uses late bindings (function
 * definitions can therefore be hoisted).
 *
 * All the function definitions are collected and moved to the top of there
 * containing scope (with statements or functions). since variable names are
 * unique (and javascript has no block scoping) vars can be safelly moved to the
 * top of their containing functions.
 *
 * TODO:Mutually reccursive functions are not hoisted
 *)
open General
open ScopeInfo

module SS=StringSet

(*w
 * The null instruction: doesn't do anything
 *)
let null=`Bloc []
module Mon=Monad.StateMonad(
 struct
  type t=AstJs.instr list
 end
)

module T=AstJs.Trav.Map(Mon)

module Hoist=T.Make(
 functor(S:T.Translation) ->
 struct
  module Super=T.Base(S)
  include Super

  (*w
   * Tests wether a given instruction captures some of the locals.
   *)
  let sepCaptured locals=
   List.partition
    ~f:(fun i ->
         let sc = ScopeInfo.instr i in
         List.exists ~f:(fun v -> ScopeInfo.isCaptured v sc) locals)

  (*w
   * Get the named of a declared function.
   *)
  let getFname = function
   | `Fundecl (name,_,_) -> name
   | _ -> assert false

  (*w
   * we can safely drop all variable definitions in the program: In javascript
   * the ^^var^^ keyword is only used to restrict the scope of a variable.
   *)
  let program p _=
   let prog,funs = Super.program p []
   in
   ((List.rev funs)@prog),[]

(*w compils a function declaration... *)
  let fundecl name args body=
   let hInstr,hFuns=instr body [] in
   let locals =
    ScopeInfo.foldDefined (ScopeInfo.instr body)
     ~init:args
     ~f:(fun i loc -> i::loc)
   in
   let capFuns,hoistFuns=sepCaptured locals hFuns in
   let fNames=
    List.fold_left hFuns
     ~init:SS.empty
     ~f:(fun s i -> SS.add (getFname i) s)
   in
   let vars = ScopeInfo.foldDefined (ScopeInfo.instr body)
    ~init:[]
    ~f:(fun i loc -> if not (SS.mem i fNames) then `Var (i,None)::loc else loc)
   in
   (`Fundecl (name,args,`Bloc ((List.rev vars)@(List.rev capFuns)@[hInstr]))),hoistFuns

  let expr e funs =
   match e with
    | `Fun(args,i) ->
       let id=TypeEnv.fresh ~hint:"hoisted" () in
       let fdecl,hFuns=fundecl id args i in
       `Ident id,
       fdecl::hFuns@funs
    |_ -> Super.expr e funs

  let instr i funs=
   match i with
    | `Fundecl (name,args,i) ->
       let fdecl,hFuns=fundecl name args i in
       null,
       fdecl::hFuns@funs
    | `WithCtx (e,i,si) ->
       (*w
        * The only purpose of using "with" is to ensure closures are correctly
        * defined, therefor we don't want to hoist function declarations any higher...
        *)
       let hInstr,hFuns=instr i [] in
       let capFuns,hoistFuns = sepCaptured si hFuns in
       `WithCtx (e,`Bloc(capFuns@[hInstr]),si),
       hoistFuns@funs
        (*w
         * We can safely drop the var def here: they will be hoisted and
         * recalculated from scopeInfo.
         *)
    | `Var (_,None) ->
       null,funs
    | `Var (i,Some e) ->
       (`Assign (`Ident i,e)),funs
    |_ -> Super.instr i funs
 end)

let pass:#Optimise.pass=
object
 method run p=Mon.run (Hoist.program p) []
 method name="hoisting"
 method description="hoisting of function and var declarations"
end
