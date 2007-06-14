open AstCpsInt

let return="$return"

module Conv=AstJs.Trav.Conv(AstCpsInt)(AstJs)(Monad.Id)
open Conv

module D(S:Par):Par=
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
    | [`Call(`Ident id,args2)] | [`Ret (`Call(`Ident id,args2))]
        when ((sameCall args args2) && (not (List.mem id args))) ->
       [],(`Ident id)
    | b ->
       let fname=Env.fresh ~hint:"CpsCont" () in
       [(`Fundecl (fname,args,`Bloc b))],(`Ident fname)

  let rec expr= function
   | `CpsFun (al,b) -> `Fun ((return::al),instr b)
   | #Old.expr as e -> Super.expr e

  (*w
    Compils an instruction with a given continuation
  *)
  and instr' i cont=
   match i with
    (*These expressions can only be converted in cps translated code*)
    | `Throw _ | `CallCC _ | `CpsCall _ -> assert false
    | `Var (i,e) ->[`Var i;`Assign (`Ident i,expr e)]@cont (*This could be done in a micro pass*)
    | `Bloc b -> bloc b cont
    | `Cps i -> cpsInstr i cont
    | `If (e,b1,b2) -> `If (expr e,`Bloc (instr' b1 []),`Bloc (instr' b2 []))::cont
    | `While (e,i) -> `While(expr e,`Bloc (instr' i []))::cont
    | #Old.instr as i -> (Super.instr i)::cont

  and cpsInstr i cont=
   match i with
    | `CpsCall (a,e,el) ->
       let head,cont=cpsCall cont a
       and el=List.map expr el in
       head@[`Call (expr e,cont::el)]
    | `Cps i -> cpsInstr i cont
    | `Throw (k,e) ->
       [`Call (expr k,[expr e])]
    | `CallCC (a,e) ->
       let head,cont=cpsCall cont a in
       head@[`Call(expr e,[cont;cont])]
    | `Ret e ->
       [`Call ((`Ident return),[expr e])]
    | `TemplateCall (el,b) ->
       let head,cont=cpsCall cont None
       and el=List.map expr el in
       head@[`TemplateCall (cont::el,b)]
    | `If (e,b1,b2) ->
       let head,cont=match (unbloc cont) with
        | [] -> [],[]
        | [`Call(`Ident id,[])] | [`Ret (`Call(`Ident id,[]))] ->
           [],[`Call (`Ident id,[])]
        | b ->
           let k=Env.fresh ~hint:"Ite" () in
           [`Fundecl(k,[],`Bloc b)],[`Call(`Ident k,[])]
       in
       let e1=instr' b1 cont in
       let e2=instr' b2 cont in
       head@[`If (expr e,`Bloc e1,`Bloc e2)]
    | `While (e,i) ->
       let k=Env.fresh ~hint:"While" () in
       let cont=[`Call ((`Ident k),[])] in
       let i=instr' i cont in
       [`Fundecl (k,[],`Bloc [`If((expr e),(`Bloc i),(`Bloc cont))]);
        `Call (`Ident k,[])]
    | `Bloc b -> bloc b cont
    | `Var _ -> assert false
    | _ -> assert false

  and cpsBloc b cont =
   List.fold_right cpsInstr b cont

  and bloc b cont =
   List.fold_right instr' b cont

  and instr i =
   let c=instr' i [] in
   `Bloc c

  and program p=
   bloc p []

 end

module rec T:Par=D(T)

let run=
 T.program
