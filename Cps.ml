include AstStd.ClosedDef
module M=Compile.Pass(
 struct
  type from=program
  type out=AstCpsHoistInt.program
  let trans=CpsTrans.run
  let print=EmitCpsHoistInt.print
  let name="cpshoist"
 end)(CpsHoistInt)

include M

