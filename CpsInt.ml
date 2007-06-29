include AstCpsInt.ClosedDef
include Compile.Pass(
 struct
  type from=program
  type out=program
  let trans=CpsPropagate.run
  let print=EmitCpsInt.print
  let name="cpsint2"
 end)(
 Compile.Pass(
  struct
   type from=program
   type out=AstJs.program
   let trans=CpsTrans2.run
   let name="rawjs"
   let print=EmitJs.print
  end)(Js)
)

