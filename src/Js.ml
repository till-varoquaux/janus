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
 let r=(EmitJs.print(opt#run p)) in
 TypeEnv.clear();
 r
type t=AstJs.program
