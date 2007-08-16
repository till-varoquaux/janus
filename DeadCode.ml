(*w
  This pass performs dead code elimination in JS code.
*)

(*w This monad allows to pass as addictional information a boolean telling
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

  (*w Processes a list of instruction.
   *
   * if an instruction in the list returns the rest of the list is
   * deadcode and therefor is dropped
   *)
  let bloc b =
   let rec aux processed= function
    | [] -> processed,false
    | x::l ->match S.instr x with
       | x,true -> x::processed,true
       | x,false -> aux (x::processed) l   in
   let l,ret=aux [] b in
   (List.rev l),ret

  let instr = function
   | `Bloc b ->
      let b',ret=bloc b in
      `Bloc b',ret
   | `Ret e ->
      let (e,_) = S.expr e in
      (`Ret e,true)
   | `If (e,b1,b2) ->
      let (e,_) = S.expr e
      and (b1,c1) = S.instr b1
      and (b2,c2) = S.instr b2 in
      (`If (e,b1,b2)),(c1 && c2)
   | i -> Super.instr i
 end)

let pass:#Optimise.pass=
object
 method run p =
  RetMon.run (D.program p)
 method name="deadcode"
 method description="deadcode elimination"
end
