include AstCpsHoistInt.ClosedDef
module M=Compile.Pass(
 struct
  type from=program
  type out=AstCpsInt.program
  let trans=CpsHoist.run
  let print=EmitCpsInt.print
  let name="cpsint"
 end)(CpsInt)

include M

