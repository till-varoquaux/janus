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

  (*w
    This compils a CPS call...
  *)
  let cpsCall cont affect=
   let args= (match affect with
               | None -> []
               | Some i -> [i])
   and fname=Env.fresh ~hint:"CpsCont" () in
   [(`Fundecl (fname,args,`Bloc cont))],(`Ident fname)

  let rec expr= function
   | `CpsFun (al,b) -> `Fun ((return::al),instr b)
   | #Old.expr as e -> Super.expr e

  (*w
    Compils an instruction with a given continuation
  *)
  and instr' i cont=
   match i with
    | `Var (i,e) ->[`Var i;`Assign (`Ident i,expr e)]@cont (*This could be done in a micro pass*)
    | `Bloc b -> bloc b cont
    | `CpsCall (a,e,el) ->
       let call,cont=cpsCall cont a
       and el=List.map expr el in
       call@[`Call (expr e,cont::el)]
    | `Cps i -> cpsInstr i cont
    | `If (e,b1,b2) -> `If (expr e,`Bloc (instr' b1 []),`Bloc (instr' b2 []))::cont
    | `While (e,i) -> `While(expr e,`Bloc (instr' i []))::cont
    | #Old.instr as i -> (Super.instr i)::cont

  and cpsInstr i cont=
   match i with
    | `Cps i -> cpsInstr i cont
    | `Ret e ->
       (`Ret (`Call ((`Ident return),[expr e])))::cont
    | `TemplateCall (el,b) ->
       let call,cont=cpsCall cont None
       and el=List.map expr el in
       call@[`TemplateCall (cont::el,b)]
    | `If (e,b1,b2) ->
       let k=Env.fresh ~hint:"Ite" () in
       let cont=[`Fundecl(k,[],`Bloc cont);
                 `Call(`Ident k,[])]
       in
       let e1=instr' b1 cont in
       let e2=instr' b2 cont in
       [`If (expr e,`Bloc e1,`Bloc e2)]
    | `While (e,i) ->
       let k=Env.fresh ~hint:"While" () in
       let cont=[`Call ((`Ident k),[])] in
       let i=instr' i cont in
       [`Fundecl (k,[],`Bloc [`If((expr e),(`Bloc i),(`Bloc cont))]);
        `Call (`Ident k,[])]
    | `Bloc b -> bloc b cont
    | `Var _ -> assert false
    | _ -> assert false

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
