include AstCpsInt.ClosedDef
include Compile.Pass(
 struct
  type from=AstCpsInt.program
  type out=AstCpsMarked.program
  let trans=CpsPropagate.run
  let print=EmitCpsMarked.print
  let name="cpsint2"
 end)(
 Compile.Pass(
  struct
   type from=AstCpsMarked.program
   type out=AstJs.program
   let trans=CpsTrans2.run
   let name="rawjs"
   let print=EmitJs.print
  end)(Js)
)

