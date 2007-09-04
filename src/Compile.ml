(*w
 * ===Compilation pass===
 *
 * This modules handles the binding code for our compilation pass, it is used to
 * allow us to compile from one of our intermediate AST's.
 *
 * TODO: handle properly dumpers and formatters in a clean way. Maybe using
 * records the PXP way.
 *
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
  val compile:t->string
  val specs:(Arg.key * Arg.spec * Arg.doc) list
 end

module Pass(In:Pass)(P:Run with type t=In.out):(Run with type t=In.from)=
 struct

  type t=In.from

  let dump=ref false

  let stop=ref false

  let specs=["-dump-"^In.name,Arg.Set dump,"<undocumented>";
             "-to-"^In.name,Arg.Set stop,"<undocumented>"
            ]@P.specs

  let compile p=
      let x=In.trans p in
      if !stop then
       In.print x
      else begin
       if !dump then
        Printf.printf "%s\n==================\n" (In.print x);
       flush stdout;
       P.compile x
      end
 end
