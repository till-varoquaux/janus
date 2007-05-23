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

module T=AstCpsInt.Trav.Map(Mon)

module D=T.Make(
 functor(S:T.T) ->
 struct
  module Super=T.Base(S)
  include Super
  let instr=function
   | `Cps i -> `Cps i,true
   | `Fundecl _ as f -> f,false
   | i -> let i,cps=Super.instr i in
     if cps then
      `Cps i,true
     else
      i,false
 end)

let run p:AstCpsInt.program=Mon.run (D.program p)
