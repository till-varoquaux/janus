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
 *)
open General

module SS=StringSet

(*w
 * This is our monad.
 *
 * It would seem logical for our monad type to be ^^t^^ (the value we are
 * computing) but since we need the value of the first branch in "bind" we need
 * to return this value in our monad our monad type is ^^'a*t^^.
 *)
module type InfoMonad=
sig
 type t
 type 'a m= 'a*t
 val return: 'a -> 'a m
 val bind: 'a m -> ('a -> 'b m) -> 'b m
end

type scopes=
  {
   defined:SS.t;
   read:SS.t;
   captured:SS.t
  }

let empty={
 defined=SS.empty;
 read=SS.empty;
 captured=StringSet.empty
}

let merge m1 m2={
 defined=StringSet.union m1.defined m2.defined;
 read=StringSet.union m1.read m2.read;
 captured=StringSet.union m1.captured m2.captured;
}

let haveCommon m1 m2=
 (SS.inter m1 m2) <> SS.empty

let rmList l s =
 List.fold_left ~f:(fun s elt -> SS.remove elt s) ~init:s l

module ScInfo=
 struct
  type 'a m='a*scopes
  let return x= x,empty
  let bind (a,scope) f =
   let b,scope'= f a in
   b,(merge scope scope')
 end

module T=AstJs.Trav.Map(ScInfo);;

module D=T.Make(
 functor(S:T.Translation) ->
 struct
  module Super=T.Base(S)
  include Super

  (*w
    The variables captured in a function whose bloc is ^^b^^ and arguments
    ^^args^^
  *)
  let getCap args b =
   let _,ctx=S.instr b in
   let read=rmList args ctx.read
   and captured=rmList args ctx.defined
   in SS.union captured read

  let expr=WeakHt.memoize
   begin function
    | `Ident i as e ->
       e,{empty with
           read=StringSet.singleton i
         }
    | `Fun(args,b) as e->
       let cap=getCap args b in
       e,{empty with
           read=cap;
           captured=cap
         }
    | e -> Super.expr e
   end

  let instr=WeakHt.memoize
   begin function
    | `Var (v,Some e) as i->
       let _,ctx = S.expr e in
       i,{ctx with
           defined=SS.add v ctx.defined
         }
    | `Var (v,None) as i ->
       i,{empty with
           defined=SS.singleton v
         }
    | `Fundecl (name,args,b) as i ->
       let cap=getCap args b in
       i,{
        read=cap;
        defined=SS.singleton name;
        captured=cap
         }
    | `WithCtx (e,b,locals) as i ->
       let _,ctx1 = S.instr b
       and _,ctx2 = S.expr e in
       let capturesLocal=List.exists ~f:(fun x -> SS.mem x ctx2.captured) locals in
       i,{
        read=SS.union ctx1.read (rmList locals ctx2.read);
        defined=SS.union ctx1.defined ctx2.defined;
        captured=
         let bCap=rmList locals ctx2.captured in
         SS.union ctx1.captured (if capturesLocal then
                                  SS.union bCap ctx1.read
                                 else
                                  bCap
                                )
       }
    | i ->Super.instr i
   end
 end
)

let expr e= snd(D.expr e)
let instr i= snd(D.instr i)

let foldDefined sc ~f ~init=
 SS.fold sc.defined ~f:f ~init:init

let foldCaptured sc ~f ~init=
 SS.fold sc.captured ~f:f ~init:init

let isCaptured v sc =
 SS.mem v sc.captured
