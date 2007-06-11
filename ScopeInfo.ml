(*w
  This module collects status information about the variables in a bloc or in an
  expression. Variables are either defined,read or captured.
*)
open General

module SS=StringSet

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
 functor(S:T.T) ->
 struct
  module Super=T.Base(S)
  include Super

  let expr=WeakHt.memoize
   begin function
    | `Ident i as e ->
       e,{empty with
           read=StringSet.singleton i
         }
    | e -> Super.expr e
   end

  let instr=WeakHt.memoize
   begin function
    | `Var v as i->
       i,{empty with
           defined=SS.singleton v
         }
    | `Fundecl (name,args,b) as i ->
       let _,ctx = instr b in
       let defined=List.fold_left (fun s elt -> SS.remove elt s) SS.empty args in
       i,{empty with
           defined=SS.singleton name;
           captured=SS.diff ctx.read defined
         }
    | i -> Super.instr i
   end

 end
)

let expr e= snd(D.expr e)
let instr i= snd(D.instr i)

let foldDefined sc f=
 SS.fold f sc.defined

let foldCaptured sc f =
 SS.fold f sc.captured

let isCaptured sc f =
 SS.mem f sc.captured
