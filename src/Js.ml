let opt=new Optimise.opt
 [
  DeadCode.pass;
  BranchMerger.pass;
  IdentFunDecls.pass;
  Hoisting.pass;
  TailRec.pass;
  TailRet.pass;
  Unbloc.pass
 ]

let specs=opt#spec
let compile p=
 print_string(EmitJs.print(opt#run p));
 TypeEnv.clear()
type t=AstJs.program