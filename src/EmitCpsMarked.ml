(*w
 * ====AstCpsMaked pretty printer====
 *)
open Printer

module Process(From:AstCpsMarked.Trav.AstDef)=
struct
 module Conv=AstCpsMarked.Trav.Conv(From)(From)(PrinterMonad)
 include Conv
 module Main(Self:Translation):PartialTranslation=
 struct

  module Tmp=EmitCpsInt.Process(From)
  module SuperIn=Tmp.In
  module Super=Tmp.Main(Self)
  include Super
  include Convenience(struct include Self module In=From end)

  let instr=function
   | `Cps i -> kwd "cps:" ^^ Self.instr i
   | #SuperIn.instr as i -> Super.instr i

 end
end

let print (p:AstCpsMarked.program):string=
 let module P=Process(AstCpsMarked) in
 let module D=P.CloseRec(P.Main) in
 Printer.toString (D.program p)
