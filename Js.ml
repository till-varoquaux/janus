let opt=new Optimise.opt
 [
  DeadCode.pass;
  BranchMerger.pass;
  IdentFunDecls.pass;
  Hoisting.pass;
  TailRec.pass;
  Unbloc.pass
 ]

let specs=opt#spec
let compile p=print_string(EmitJs.print(opt#run p))
type t=AstJs.program
