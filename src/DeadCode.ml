(*w
 *  ===Dead code removal===
 *
 * This pass performs dead code elimination in JS code. It is avery simple pass
 * and will only remove code situated directly after a ^^return^^
 * instruction.
 *
 * A more involved future evolution would be ro replace this pass alltogether
 * with a
 * [[http://en.wikipedia.org/wiki/Sparse_conditional_constant_propagation|sparse
 * constant propagation]] pass. This would require implementing SSA somewhere in
 * the compiler.
 *
 * **Grade** C
 *)

(*w
 * This monad allows to pass as addictional information a boolean telling
 * wether the generated code returns (the following code in the same block will
 * be dead code)
 *)
module RetMon=
struct
 type 'a m = 'a * bool
 let return a=(a,false)
 let bind (x,_) f = (f x)
 let run (x,_) = x
end

module T=AstJs.Trav.Map(RetMon);;
module D=T.Make(functor(S:T.Translation) ->
 struct
  module Super=T.Base(S)
  include Super

  (*w
   * Processes a list of instruction.
   *
   * if an instruction in the list returns the rest of the list is
   * deadcode and therefor is dropped
   *)
  let rec bloc=function
   | [] -> [],false
   | h::t -> match S.instr h with
      | x,true -> [x],true
      | x,false ->
         let t',ret = bloc t in
         (x::t'),ret

  let instr = function
   | `Bloc b ->
      let b',ret=bloc b in
      `Bloc b',ret
   | `Ret (Some e) ->
      let (e,_) = S.expr e in
      `Ret (Some e),true
   | (`Ret None | `Continue _ | `Break _ )as i -> i,true
   | `If (e,b1,b2) ->
      let (e,_) = S.expr e
      and (b1,c1) = S.instr b1
      and (b2,c2) = S.instr b2 in
      (`If (e,b1,b2)),(c1 && c2)
   | `While (e,b) ->
      let (e,_) = S.expr e
      and (b,_) = S.instr b in
      `While (e,b),false
   | i -> Super.instr i
 end)

let pass:#Optimise.pass=
object
 method run p =
  RetMon.run (D.program p)
 method name="deadcode"
 method description="deadcode elimination"
end
