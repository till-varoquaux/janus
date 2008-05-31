(*w
 * ====Scoping information====
 * This module collects status information about the variables in a bloc or in
 * an expression. Variables are either defined,read or captured.
 *
 * Although it is implemented as a pass it isn't intended to be ran as one, we
 * are just reusing or framework here in order to avoid writing boilerplate code
 * for the navigation. Computation cost is amortized using memoization.
 *
 * Memoization cannot be added in the monads. See "generalizing monads to
 * arrows" for a eventual solution.
 *
 * FUTURE:
 * Extend this module to liveness information.
 *
 * **Grade** D
 *)
open General

module SS =
struct
 include StringSet

 let rmList l s =
  List.fold_left ~f:(fun s elt -> remove elt s) ~init:s l
end

type scope={
 defined:SS.t; (*defined variables*)
 read:SS.t; (*read variables*)
 captured:SS.t; (*variables captured in a closure*)
 usedLabels:SS.t (*labels used (either in a break or acontinue statement)*)
  }

let empty={
 defined=SS.empty;
 read=SS.empty;
 captured=SS.empty;
 usedLabels=SS.empty
}

let merge m1 m2={
 defined=SS.union m1.defined m2.defined;
 read=SS.union m1.read m2.read;
 captured=SS.union m1.captured m2.captured;
 usedLabels=SS.union m1.usedLabels m2.usedLabels;
}

(*w
 * This is our monad.
 *
 * It would seem logical for our monad type to be ^^t^^ (the value we are
 * computing) but since we need the value of the first branch in "bind" we need
 * to return this value in our monad our monad type is ^^'a*t^^.
 *)
module ScInfo=
 struct
  type 'a m = 'a*scope
  let return x = x,empty
  let bind (a,scope) f =
   let b,scope'= f a in
   b,(merge scope scope')
 end

module Trav=AstJs.Trav.Map(ScInfo);;

include Trav.CloseRec(
 functor(Self:Trav.Translation) ->
 struct
  module Super=Trav.Base(Self)
  include Super

  let lift f =
   let fMem = WeakHt.memoize f in
   fun h ->
    h,(fMem h)

  (*w
   * The variables captured in a function whose bloc is ^^b^^ and arguments
   * ^^args^^
   *)
  let getCap args b =
   let _,ctx = Self.instr b in
   let read = SS.rmList args ctx.read
   and captured = SS.rmList args ctx.defined
   in SS.union captured read

  let expr=lift
   begin function
    | `Ident i ->
       {empty with read=StringSet.singleton i}
    | `Fun(args,b) ->
       let cap=getCap args b in
       {empty with
         read=cap;
         captured=cap}
    | e -> snd (Super.expr e)
   end

  let instr=lift
   begin function
    | `Var (v,Some e) ->
       let _,ctx = Self.expr e in
       {ctx with defined=SS.add v ctx.defined }
    | `Var (v,None) ->
       {empty with defined=SS.singleton v }
    | `Fundecl (name,args,b) ->
       let cap=getCap args b in
       {read=cap;
        defined=SS.singleton name;
        captured=cap;
        usedLabels=SS.empty}
    | `WithCtx (e,b,locals) ->
       let _,ctx1 = Self.instr b
       and _,ctx2 = Self.expr e in
       let capturesLocal=List.exists ~f:(fun x -> SS.mem x ctx2.captured) locals
       in
       {read=SS.union ctx1.read (SS.rmList locals ctx2.read);
        defined=SS.union ctx1.defined ctx2.defined;
        captured=
         (let bCap=SS.rmList locals ctx2.captured in
          SS.union ctx1.captured (if capturesLocal then
                                   SS.union bCap ctx1.read
                                  else
                                   bCap
                                 ));
         usedLabels=SS.union ctx1.usedLabels ctx2.usedLabels}
    | `Continue (Some lbl) | `Break (Some lbl) ->
       {empty with usedLabels=SS.singleton lbl}
    | `Labeled (lbl,subI) ->
       let _,ctx=Self.instr subI in
       {ctx with usedLabels=SS.remove lbl ctx.usedLabels}
    | i ->snd (Super.instr i)
   end
 end)

let expr e = snd(expr e)
let instr i = snd(instr i)

let foldDefined i ~f ~init =
 SS.fold (instr i).defined ~f:f ~init:init

let foldCaptured i ~f ~init =
 SS.fold (instr i).captured ~f:f ~init:init

let isCaptured v i =
 SS.mem v (instr i).captured

let isUsedLabel l i =
 SS.mem l (instr i).usedLabels
