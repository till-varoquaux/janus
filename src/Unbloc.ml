(*w
 * ====Bloc expansions====
 * Blocs are list of instructions enclosed in brackets. This extension expands
 * all blocs (ie rmoves the backets) when they without changing the semmantic of
 * the program.
 *)

module Id=Monad.Id
module T=AstJs.Trav.Map(Id);;

module D=T.Make(
 functor(S:T.Translation) ->
 struct
  module Super=T.Base(S)
  include Super

  (*w
   * This function expands all the blocs in an instruction list.
   *)
  let unroll=
   let rec aux= function
    | [] -> []
    | (`Bloc b)::t -> (aux b)@(aux t)
    | (h:T.i AstJs.Gram.instr)::t -> (S.instr h)::(aux t)
   in
   aux

  (*w
   * Used to delete case where bloc contain only
   * a single instruction.
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
   | `While(e,b) ->
      `While(e,blocOrInstr b)
   | i -> Super.instr i
  let program =
   unroll
 end
)

let pass:#Optimise.pass=
object
 method run p =
  D.program p
 method name="unbloc"
 method description="bloc simplification"
end
