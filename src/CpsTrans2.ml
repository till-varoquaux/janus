(*w
 * ====Cps conversion 2/2 ====
 * This is the second part of the cps conversion. In the input code all the
 * operations to convert are marked.
 *
 * None of the instructions have CPS function calls as
 * there argumenent (that is: there are no Cps expression). This is reminiscent
 * of [[http://en.wikipedia.org/wiki/Administrative_normal_form|A normal form]].
 *
 * Cps instruction are converted bottom up: they take a list of instructions
 * representing there continuation, we must therefor proceed from the the end
 * towards the beginning.
 *
 * Since this is a partial Cps conversion we have three cases for the
 * instructions we convert:
 *
 * - ^^CpsInstr^^: The instruction needs to be  CPS converted
 * - ^^MaybeCpsInstr^^: This instruction needs to be converted only if it is
 * marked
 * - ^^Instr^^: This is a none CPS instruction.
 *
 * **TODO**: There are some ad-hoc hacks to avoid introducing unnecessary lambda
 * terms. These should be removed and replaced by more general javascript
 * optimisation passes (administrative reductions).
 *
 * **Grade** D
 *)

(*w
 * The continuation name
 *)
let contName="$cont"

module Conv=AstJs.Trav.TranslateFrom(AstCpsMarked)(Monad.Id)

module D=Conv.CloseRec(
 functor(S:Conv.Translation) ->
 struct
  module Super=Conv.Base(S)
  include Super

  let sameCall args el=
   try
    List.for_all2 (fun a e -> e=(`Ident a)) args el
   with Invalid_argument _ -> false

  let rec flattenBloc= function
   | [] -> []
   | `Bloc b::l -> b@(flattenBloc l)
   | i::l -> i::(flattenBloc l)

  (*w
   * This reifies the continuation as a function.
   *
   * It returns an eventual function (which is the continuation) and an
   * identifier which is the name of the continuation.
   *
   * - ^^affect^^ is the name of the variable where the return value will be
   * stored
   *)
  let reifyContinuation cont affect=
   let args= (match affect with
               | None -> []
               | Some i -> [i])
   in
   match (flattenBloc cont) with
     (*w
      * we don't need to make a new function, the continuation is already a
      * function...
      *
      * We could return an anonymous function instead, however having the a
      * for the continuation allows us to identify it when it has been
      * duplicated.
      *)
    | ([`Call(`Ident id,args2);`Ret None] | [`Call(`Ident id,args2)])
      when ((sameCall args args2) && (not (List.mem id args))) -> [],(`Ident id)
    | b ->
       let fname=TypeEnv.fresh ~hint:"$CpsCont" () in
       [(`Fundecl (fname,args,`Bloc b))],(`Ident fname)

  let rec expr= function
   | `CpsFun (al,b) -> `Fun ((contName::al),`Bloc (maybeCpsInstr b []))
   | #Conv.In.expr as e -> Super.expr e

  and instr= function
    (*These expressions can only be converted in cps translated code*)
   | `Throw _ | `CallCC _ | `CpsCall _ | `CpsRet _ | `Cps _ -> assert false
   | #Conv.In.instr as i -> Super.instr i

  and cpsInstr ?(top=false) i cont=
   let cps=(* this should be merged with the reification of the continuation*)
    if top then
     fun head i -> head@[i]
    else
     fun head i -> head@[i;`Ret None]
   in
   match i with

    (*Cps specific instructions*)
    | `CpsCall (a,e,el) ->
       let head,cont=reifyContinuation cont a
       and el=List.map expr el in
       cps head (`Call (expr e,cont::el))
    | `CallCC (a,e,el) ->
       let head,cont=reifyContinuation cont a
       and el=List.map expr el in
       cps head (`Call (expr e,cont::cont::el))
    | `Throw (k,e) ->
       cps [] (`Call (expr k,[expr e]))
    | `CpsRet (Some e) ->
       cps [] (`Call (`Ident contName,[expr e]))
    | `CpsRet None ->
       cps [] (`Call (`Ident contName,[]))

    (*Instruction containing block and therefor needing to be convereted*)
    | `If (e,b1,b2) ->
       let head,cont=match (flattenBloc cont) with
        | [] -> [],[]
        | [`Call(`Ident id,[])] | [`Ret (Some (`Call(`Ident id,[])))] ->
           [],[`Call (`Ident id,[])]
        | b ->
           let k=TypeEnv.fresh ~hint:"Ite" () in
           [`Fundecl(k,[],`Bloc b)],[`Call(`Ident k,[])]
       in
       let e1=maybeCpsInstr ~top:top b1 cont in
       let e2=maybeCpsInstr ~top:top b2 cont in
       head@[`If (expr e,`Bloc e1,`Bloc e2)]
    | `While (e,i) ->
       let k=TypeEnv.fresh ~hint:"CpsWhile" () in
       let cont'=[`Call ((`Ident k),[])] in
       let i=maybeCpsInstr ~top:top i cont' in
       [`Fundecl (k,[],`Bloc [`If((expr e),(`Bloc i),(`Bloc cont))]);
        `Call (`Ident k,[])]
    | `Bloc b -> cpsBloc ~top:top b cont

    (*These instructions are by essence non cps, if they have been marked
         something must have gone wrong*)
    | `Var _ | `Assign _ | `Call _ | `Expr _ | `Ret _ | `Cps _ -> assert false

  (*w
   * Compile an instruction that could be either a cps instruction or a simple
   * one. ^^cont^^ is the continuation (given as a list of instructions), and
   * top is a boolean telling us wether we are in a function or not.
   *)
  and maybeCpsInstr ?(top=false) i cont =
   match i with
    | `Cps i -> cpsInstr ~top:top i cont
    | `Throw _ | `CallCC _ | `CpsCall _
    | `CpsRet _ -> assert false (*These should be marked*)
    | #Conv.In.instr as i -> (Super.instr i)::cont

  and cpsBloc ?(top=false) b cont =
   List.fold_right (maybeCpsInstr ~top:top) b cont

  and program p=
   cpsBloc ~top:true p []

 end)

let run=
 D.program
