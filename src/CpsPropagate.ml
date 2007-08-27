module PropMonad=
struct
 type 'a m= ('a*bool)
 let run (f:'a m) : 'a = fst f
 let bind ((x,cps1):'a m) (g:'a -> 'b m) : 'b m=
  let res,cps2=g x in
  res,(cps1 || cps2)
  ;;
 let return a=a,false
end;;

module Mon=PropMonad
module Conv=AstCpsMarked.Trav.TranslateFrom(AstCpsInt)(Mon)
open Conv
module D=Conv.Make(
 functor(S:Translation) ->
 struct
  module Super=Conv.Base(S)
  include Super
  let expr=function
   | `Fun (args,i) ->
      `Fun(args,fst (S.instr i)),false
   | `CpsFun (args,i) ->
      `CpsFun(args,fst (S.instr i)),false
   | e -> Super.expr e
  let instr=function
   | (`CpsCall _ | `CpsRet _
     | `CallCC _ | `Throw _
     | `Abort) as i ->
      `Cps (fst(Super.instr i)),true
   | `Cps i -> `Cps (fst (S.instr i)),true
   | #In.instr as i ->
      let i,cps=Super.instr i in
      if cps then
       `Cps i,true
      else
       i,false
 end)

let run p:AstCpsMarked.program=Mon.run (D.program p)
