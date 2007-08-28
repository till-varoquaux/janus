(*w
 * ====AstCpsMaked pretty printer====
 *)
open Printer

module Process(From:AstCpsMarked.Trav.AstDef)=
struct
 module Conv=AstCpsMarked.Trav.Conv(From)(From)(PrinterMonad)
 include Conv
 module Main(S:Translation):PartialTranslation=
 struct

  module Tmp=EmitCpsInt.Process(From)
  module Super=Tmp.Main(S)
  include Super
  include Convenience(struct include S module In=From end)

  let instr=function
   | `Cps i -> kwd "cps:" ^^ S.instr i
   | #In.instr as i -> Super.instr i

 end
end

module P=Process(AstCpsMarked)
module rec D:P.Translation=P.Main(D)

let print (p:AstCpsMarked.program):string=
 Printer.toString (D.program p)
