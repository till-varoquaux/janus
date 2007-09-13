(*w
 * ====Remove unneeded function declarations====
 *
 * We only remove local declarations since we consider anything defined at
 * toplevel could be usefull in a library for instance.
 *)

open General
module ScopeMonad=
 struct
  type 'a m=StringSet.t -> 'a
  let return v = fun _ -> v
  let bind v f = fun unused -> f (v unused) unused
  let run v = v StringSet.empty
 end

module Conv=AstJs.Trav.Map(ScopeMonad)

module D=Conv.CloseRec(
 functor(Self:Conv.Translation) ->
 struct
  module Super=Conv.Base(Self)
  include Super

  (*w
   * Returns the set of variables defined in an instruction but not used in this
   * instruction. This doesn't tell us wether these variables are used
   * elsewhere.
   *)
  let unusedVars i=
   let (-) = StringSet.diff
   and (+) = StringSet.union
   and {ScopeInfo.read=read;
        defined=defined;
        captured=captured
       }=ScopeInfo.instr i
   in
   defined-(captured+read)

  let expr e unused=
   match e with
    | `Fun (args,body) -> `Fun (args,Self.instr body (unusedVars body))
    | e -> Super.expr e unused

  let instr (i:AstJs.instr) unused=
   match i with
    | `Fundecl(name,_,_) when StringSet.mem name unused -> `Bloc []
    | `Fundecl(name,args,body) ->
       `Fundecl (name,args,Self.instr body (unusedVars body))
    | `Labeled(lbl,i) ->
       let i=Self.instr i unused in
       if ScopeInfo.isUsedLabel lbl i then
        `Labeled(lbl,i)
       else
        i
    | i -> Super.instr i unused
 end)

let pass:#Optimise.pass=
object
 method run p=ScopeMonad.run (D.program p)
 method name="Unneeded"
 method description="Remove unneeded internal functions"
end
