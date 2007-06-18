module CpsMonad=
struct
 type 'a m = 'a * (AstCpsInt.instr list)
 let return a=(a,[])
 let bind (x,ctx) f =
  let res,ctx2=(f x) in
  res,(ctx@ctx2)
 let run (x,_) = x
end

module Conv=AstCpsInt.Trav.TranslateFrom(AstCpsHoistInt)(CpsMonad)
open Conv
module D(S:Translation)=
 struct
  module Super=Base(S)
  include Super
  (*FIXME: check why Old.expr doesn't work*)
  type e'=i AstCpsInt.Gram.expr
  let expr: AstCpsHoistInt.expr -> AstCpsInt.expr CpsMonad.m= function
   | `Hoist (e,i) ->
       let e',ctx=S.expr e
       and i',_=S.instr i in
       assert ((snd (S.instr i))= []);
       e',(i'::ctx)
   | #e' as e -> Super.expr e
      (* TODO handle laziness in and and or *)

  let instr= function
   | `Cps i ->
      (match Super.instr i with
        | i,[] -> `Cps i,[]
        | i,ctx -> `Bloc (ctx@[`Cps i]),[])
   | i ->
      (match Super.instr i with
        | i,[] -> i,[]
        | i,ctx -> `Bloc (ctx@[i]),[])
 end

module rec T:Translation=D(T)

let run p=
 CpsMonad.run (T.program p)
