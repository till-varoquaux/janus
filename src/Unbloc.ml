(*w
 * ====Bloc expansions====
 * Blocs are list of instructions enclosed in brackets. This extension expands
 * all blocs (ie rmoves the backets) when they without changing the semmantic of
 * the program.
 *)

module Conv=AstJs.Trav.Map(Monad.Id);;

module D=Conv.CloseRec(
 functor(Self:Conv.Translation) ->
 struct
  module Super=Conv.Base(Self)
  include Super

  (*w
   * This function expands all the blocs in an instruction list.
   *)
  let unroll=
   let rec aux= function
    | [] -> []
    | (`Bloc b)::t -> (aux b)@(aux t)
    | (h:Conv.In.instr)::t -> (Self.instr h)::(aux t)
   in
   aux

  (*w
   * Used to delete case where bloc contain only
   * a single instruction.
   *)
  let blocOrInstr = function
   | `Bloc b ->
      (match unroll b with
        | [i] -> Self.instr i
        | l -> `Bloc l)
   | i -> Self.instr i

  let instr = function
   | `Bloc b -> `Bloc (unroll b)
   | `If (e,b1,b2) ->
      let b1=blocOrInstr b1
      and b2=blocOrInstr b2 in
      `If (Self.expr e,b1,b2)
   | `While(e,b) ->
      `While(e,blocOrInstr b)
   | i -> Super.instr i
  let program =
   unroll
 end)

let pass:#Optimise.pass=
object
 method run p =
  D.program p
 method name="unbloc"
 method description="bloc simplification"
end
