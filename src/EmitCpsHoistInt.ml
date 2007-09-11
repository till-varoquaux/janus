(*w
  ====AstCpsInt pretty printer====
*)
open Printer

module Process(From:AstCpsHoistInt.Trav.AstDef)=
struct
 module Conv=AstCpsHoistInt.Trav.Conv(From)(From)(PrinterMonad)
 include Conv
 module Main(S:Translation):PartialTranslation=
 struct

  module Tmp=EmitCpsInt.Process(From)
  module SuperIn=Tmp.In
  module Super=Tmp.Main(S)
  include Super
  include Convenience(struct include S module In=From end)

  let expr=function
   | `Hoist(e,i) -> S.expr e ^^ (brace (brace (S.instr i)))
   | #SuperIn.expr as e -> Super.expr e

 end
end

module P=Process(AstCpsHoistInt)
module rec D:P.Translation=P.Main(D)

let print (p:AstCpsHoistInt.program):string=
 Printer.toString (D.program p)
