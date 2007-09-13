(*w
 * ====AstCpsInt pretty printer====
 *)
open Printer

module Process(From:AstCpsHoistInt.Trav.AstDef)=
struct
 module Conv=AstCpsHoistInt.Trav.Conv(From)(From)(PrinterMonad)
 include Conv
 module Main(Self:Translation):PartialTranslation=
 struct

  module Tmp=EmitCpsInt.Process(From)
  module SuperIn=Tmp.In
  module Super=Tmp.Main(Self)
  include Super
  include Convenience(struct include Self module In=From end)

  let expr=function
   | `Hoist(e,i) -> Self.expr e ^^ (brace (brace (Self.instr i)))
   | #SuperIn.expr as e -> Super.expr e

 end
end

let print (p:AstCpsHoistInt.program):string=
 let module P=Process(AstCpsHoistInt) in
 let module D=P.CloseRec(P.Main) in
 Printer.toString (D.program p)
