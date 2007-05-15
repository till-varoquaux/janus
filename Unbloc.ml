(*Performs dead code elimination*)

module Id=Monad.Id
module T=AstJs.Trav.Map(Id);;

module D=T.Make(
 functor(S:T.T) ->
 struct
  module Super=T.Base(S)
  include Super

  (**
     This function expands all the blocs in a instruction list.
  *)
  let unroll=
   let rec aux= function
    | [] -> []
    | (`Bloc b)::t -> (aux b)@(aux t)
    | (h:T.i AstJs.Gram.instr)::t -> (S.instr h)::(aux t)
   in
   aux

  (**
     Returns a single instruction, used to delete case where bloc contain only
     a single instruction.
  *)
  let blocOrInstr = function
   | `Bloc b ->
      (match unroll b with
        | [i] -> S.instr i
        | l -> `Bloc l)
   | i -> S.instr i

  let instr = function
   | `Bloc b -> `Bloc (unroll b)
   | `If (e,b1,b2) ->
      let b1=blocOrInstr b1
      and b2=blocOrInstr b2 in
      `If (S.expr e,b1,b2)
   | i -> Super.instr i
  let program =
   unroll
 end
)

let run=
 D.program
