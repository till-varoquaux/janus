(*w
 *  ====Suppresion of unneeded return instructions====
 * This pass identify the empty return placed at the end of a function. Since
 * functions call return implicitly when they end we might as well remove them.
 *
 * Unused continues in while loops and labels are also removed.
 *)

type pos = LoopTail | FunTail | Normal
module EndMon=
 struct
  type 'a m = pos -> string option -> 'a
  let return m = fun _ _ -> m
  let bind v f = fun _ lbl -> f (v Normal lbl) Normal lbl
  let run v = v Normal None
 end

module Mapper=AstJs.Trav.Map(EndMon)
module D'=Mapper.CloseRec(
 functor(Self:Mapper.Translation) ->
 struct
  module Super=Mapper.Base(Self)
  include Super

  let rec bloc b pos loopName =
   match b with
   | [] -> []
   | h::t ->
      let cont=bloc t pos loopName in
      let hPos= match cont with [] -> pos | _ -> Normal
      in match Self.instr h hPos loopName with
       | `Bloc [] -> cont
       | i->i::cont

  let instr i pos loopName =
   match i with
    | `Fundecl(name,args,body) ->
       `Fundecl(name,args,Self.instr body FunTail None)
    | `Ret None when pos=FunTail -> `Bloc []
    | `Continue (Some i) when (pos=LoopTail) && (loopName=Some i) -> `Bloc []
    | `Continue None when (pos=LoopTail) -> `Bloc []
    | `Continue (Some i) when (loopName=Some i) -> `Continue None
    | `If (e,i1,i2) ->
       let e'=Self.expr e Normal loopName
       and i1'=Self.instr i1 pos loopName
       and i2'=Self.instr i2 pos loopName
       in `If(e',i1',i2')
    | `WithCtx(e,i,ids) ->
       let e'=Self.expr e Normal loopName
       and i'=Self.instr i pos loopName in
       `WithCtx(e',i',ids)
    | `Labeled(lbl,`While(e,i)) ->
       let e'=Self.expr e Normal (Some lbl)
       and i'=Self.instr i LoopTail (Some lbl) in
       `While(e',i')
    | `While(e,i) ->
       let e'=Self.expr e Normal None
       and i'=Self.instr i LoopTail None in
       `While(e',i')
    | `Bloc b -> `Bloc (bloc b pos loopName)
    | i -> Super.instr i pos loopName

  let expr e pos loopName =
    match e with
     | `Fun (args,body) ->
        `Fun(args,Self.instr body FunTail None)
     | e -> Super.expr e pos loopName
 end)

let pass:#Optimise.pass=
object
 method run p = EndMon.run (D'.program p)
 method name="tailret"
 method description="Remove all the trailling empty \"return\" statements."
end
