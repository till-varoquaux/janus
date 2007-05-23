open AstCpsInt

let return="$return"

let cpsCall cont affect=
 let args= (match affect with
          | None -> []
          | Some i -> [i])
 and fname=Env.fresh ~hint:"CpsCont" () in
 [(`Fundecl (fname,args,`Bloc cont))],(`Ident fname)

let rec cpsInstr i cont=
 match i with
  | `Cps i -> cpsInstr i cont
  | `Ret e ->
     (`Ret (`Call ((`Lval(`Ident return)),[e])))::cont
  | `Fundecl (i,al,b) ->
     (`Fundecl (i,(return::al),fbloc b))::cont;
  | `TemplateCall (el,b) ->
     let call,cont=cpsCall cont None in
     call@[`TemplateCall (`Lval cont::el,b)]
  | `If (e,b1,b2) ->
     let k=Env.fresh ~hint:"Ite" () in
     let cont=[`Fundecl(k,[],`Bloc cont);
              `Call(`Lval(`Ident k),[])]
     in
     let e1=instr b1 cont in
     let e2=instr b2 cont in
     [`If (e,`Bloc e1,`Bloc e2)]
  | `While (e,i) ->
     let k=Env.fresh ~hint:"While" () in
     let cont=[`Call ((`Lval(`Ident k)),[])] in
     let i=instr i cont in
     [`Fundecl (k,[],`Bloc [`If(e,(`Bloc i),(`Bloc cont))]);
      `Call (`Lval(`Ident k),[])]
  | _ -> assert false

and instr i cont=
 match i with
  | `Var (i,e) ->[`Var i;`Assign (`Ident i,e)]@cont (*This could be done in a micro pass*)
  | `Bloc b -> bloc b cont
  | `CpsCall (a,e,el) ->
     let call,cont=cpsCall cont a in
     call@[`Call (e,`Lval cont::el)]
  | `Fundecl (i,al,b) -> (`Fundecl(i,al,fbloc b))::cont;
  | `Cps i -> cpsInstr i cont
  | `If (e,b1,b2) -> `If (e,`Bloc (instr b1 []),`Bloc (instr b2 []))::cont
  | `While (e,i) -> `While(e,`Bloc (instr i []))::cont
  | `Assign _ | `Call _ | `Ret _ | `TemplateCall _ as i -> (i:>AstJs.instr)::cont

and fbloc b =
 let c=instr b [] in
 `Bloc c

and bloc b cont =
 List.fold_right instr b cont

and program p=
 bloc p []

let run=
 program
