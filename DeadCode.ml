(*Performs dead code elimination*)

module RetMon=
struct
 type 'a m = 'a * bool
 let return a=(a,false)
 let bind (x,_) f = (f x)
 let run (x,_) = x
end

module T=AstJs.Trav.Map(RetMon);;

module D=T.Make(
 functor(S:T.T) ->
 struct
  module Super=T.Base(S)
  include Super
  let instr = function
   | `Ret e ->
      let (e,_) = S.expr e in
      (`Ret e,true)
   | `If (e,b1,b2) ->
      let (e,_) = S.expr e
      and (b1,c1) = S.instr b1
      and (b2,c2) = S.instr b2 in
      (`If (e,b1,b2)),(c1 && c2)
   | i -> Super.instr i
  let bloc b =
   let rec aux res= function
    | [] -> res,false
    | x::l ->
       match S.instr x with
        | x,true ->
           x::res,true
        | x,false -> aux (x::res) l   in
   let l,ret=aux [] b in
   (List.rev l),ret
 end
)

let run p =
 RetMon.run (D.program p)
