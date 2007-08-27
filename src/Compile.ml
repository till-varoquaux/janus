(*w
  ==Compilation pass==

  This function codes for a compilation pass, it is used to allow us to compile
  from one of our intermediate AST's.
*)

module type Pass=
 sig
  type from
  type out
  val trans:from->out
  val name:string
  val print:out->string
 end
module type Run=
 sig
  type t
  val compile:t->unit
  val specs:(Arg.key * Arg.spec * Arg.doc) list
 end

module Pass(In:Pass)(P:Run with type t=In.out):(Run with type t=In.from)=
 struct
  type t=In.from
  let dump=ref false
  let specs=["-dump-"^In.name,Arg.Set dump,"<undocumented>"]@P.specs
  let compile=
     fun p ->
      flush stdout;
      let x=In.trans p in
      if !dump then
       Printf.printf "%s\n==================\n" (In.print x);
      flush stdout;
      P.compile x
 end
