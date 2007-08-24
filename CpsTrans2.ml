let return="$cont"

module Conv=AstJs.Trav.TranslateFrom(AstCpsMarked)(Monad.Id)
open Conv

module D=Conv.Make(
 functor(S:Translation) ->
 struct
  module Super=Base(S)
  include Super

  (*w
    Our language is typeless
  *)
  let ty _ = assert false

  let sameCall args el=
   try
    List.for_all2 (fun a e -> e=(`Ident a)) args el
   with Invalid_argument _ -> false

  let rec unbloc= function
   | [] -> []
   | `Bloc b::l -> b@(unbloc l)
   | i::l -> i::(unbloc l)

  (*w
    This compiles a CPS call...

    It returns an eventual function (which is the continuation) and an
    identifier which is the name of the continuation.

    ^^affect^^ is the name of the variable where the return value will be stored
  *)
  let cpsCall cont affect=
   let args= (match affect with
               | None -> []
               | Some i -> [i])
   in
   match (unbloc cont) with
     (*w
       we don't need to make a new function, the continuation is already a
       function...

       We could return an expression but the hoisting pass would have to
       recognize duplicate anonymous functions.
     *)
    | [`Call(`Ident id,args2)] | [`Ret (Some (`Call(`Ident id,args2)))]
        when ((sameCall args args2) && (not (List.mem id args))) ->
       [],(`Ident id)
    | b ->
       let fname=TypeEnv.fresh ~hint:"CpsCont" () in
       [(`Fundecl (fname,args,`Bloc b))],(`Ident fname)

  let rec expr= function
   | `CpsFun (al,b) -> `Fun ((return::al),maybeCpsInstr b)
   | #In.expr as e -> Super.expr e

  and instr i =
   let c=instr' i [] in
   `Bloc c

  and cpsInstr i =
   let c=cpsInstr' i [] in
   `Bloc c

  and maybeCpsInstr i=
   let c=maybeCpsInstr' i [] in
   `Bloc c

  (*w
    Compils an instruction with a given continuation
  *)
  and instr' i cont=
   match i with
    (*These expressions can only be converted in cps translated code*)
    | `Throw _ | `CallCC _ | `CpsCall _ | `CpsRet _ | `CpsTemplateCall _ | `Abort -> assert false
    | `Bloc b -> bloc b cont
    | `Cps i -> cpsInstr' i cont
    | `If (e,b1,b2) -> `If (expr e,`Bloc (instr' b1 []),`Bloc (instr' b2 []))::cont
    | `While (e,i) -> `While(expr e,`Bloc (instr' i []))::cont
    | #In.instr as i -> (Super.instr i)::cont

  and cpsInstr' i cont=
   match i with
    | `TemplateCall _ | `Ret _ -> assert false
    | `CpsCall (a,e,el) ->
       let head,cont=cpsCall cont a
       and el=List.map expr el in
       head@[`Call (expr e,cont::el)]
    | `Cps i -> cpsInstr' i cont
    | `Throw (k,e) ->[`Call (expr k,[expr e]);`Ret None]
    | `Abort -> [`Ret None]
    | `CallCC (a,e,el) ->
       let head,cont=cpsCall cont a
       and el=List.map expr el in
       head@[`Call(expr e,cont::cont::el)]
    | `CpsRet (Some e) ->
       [`Ret (Some(
               `Call ((`Ident return),[expr e])
             ))
       ]
    | `CpsRet None ->
       [`Ret (Some(
               `Call ((`Ident return),[])
              ))
       ]
    | `CpsTemplateCall (el,b) ->
       let head,cont=cpsCall cont None
       and el=List.map expr el in
       head@[`TemplateCall (cont::el,b)]
    | `If (e,b1,b2) ->
       let head,cont=match (unbloc cont) with
        | [] -> [],[]
        | [`Call(`Ident id,[])] | [`Ret (Some (`Call(`Ident id,[])))] ->
           [],[`Call (`Ident id,[])]
        | b ->
           let k=TypeEnv.fresh ~hint:"Ite" () in
           [`Fundecl(k,[],`Bloc b)],[`Call(`Ident k,[])]
       in
       let e1=instr' b1 cont in
       let e2=instr' b2 cont in
       head@[`If (expr e,`Bloc e1,`Bloc e2)]
    | `While (e,i) ->
       let k=TypeEnv.fresh ~hint:"While" () in
       let cont'=[`Call ((`Ident k),[])] in
       let i=instr' i cont' in
       [`Fundecl (k,[],`Bloc [`If((expr e),(`Bloc i),(`Bloc cont))]);
        `Call (`Ident k,[])]
    | `Bloc b -> bloc b cont
    | `Var _ | `Assign _ | `Call _ | `Expr _ -> assert false

  and maybeCpsInstr' i cont =
   match i with
    | `Cps i -> cpsInstr' i cont
    | _ -> instr' i cont

  and cpsBloc b cont =
   List.fold_right cpsInstr' b cont

  and bloc b cont =
   List.fold_right instr' b cont

  and program p=
   List.fold_right maybeCpsInstr' p []

 end)

let run=
 D.program
