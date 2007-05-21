open AstCpsInt

let return="$return"

let cpsCall inf affect=
 let args= (match affect with
          | None -> []
          | Some i -> [i])
 and fname=Env.fresh ~hint:"CpsCont" () in
 [(`Fundecl (fname,args,`Bloc inf))],(`Ident fname)

let rec instr (i:AstCpsInt.instr) inf=
 match i with
  | `Var (i,e) -> let i:Env.id =i in [`Var i;`Assign (`Ident i,e)]@inf
                                     (*This could be done in a micro pass*)
  | `Bloc b -> bloc b inf
  | `CpsTemplateCall (el,b) ->
     let call,cont=cpsCall inf None in
     call@[`TemplateCall (`Lval cont::el,b)]
  | `CpsCall (a,e,el) ->
     let call,cont=cpsCall inf a in
     call@[`Call (e,`Lval cont::el)]
  | `Cpsdecl (i,al,b) ->
     (`Fundecl (i,(return::al),fbloc b))::inf;
  | `Fundecl (i,al,b) ->
     (`Fundecl(i,al,fbloc b))::inf;
      (*optimize me this...*)
  | `If (e,b1,b2) (*when (Env.cps inf.env)*)  ->
     let k=Env.fresh ~hint:"Ite" () in
     let inf=[`Fundecl(k,[],`Bloc inf);
              `Call(`Lval(`Ident k),[])]
     in
     let e1=instr b1 inf in
     let e2=instr b2 inf in
     [`If (e,`Bloc e1,`Bloc e2)]
  | `While (e,i) (*when (Env.cps inf.env)*) ->
     let k=Env.fresh ~hint:"While" () in
     let cont=[`Call ((`Lval(`Ident k)),[])] in
     let i=instr i cont in
     [`Fundecl (k,[],`Bloc [`If(e,(`Bloc i),(`Bloc inf))]);
      `Call (`Lval(`Ident k),[])]
  | `CpsRet e ->
     (`Call ((`Lval(`Ident return)),[e]))::inf
  | `Assign _ | `Call _ | `Ret _ | `TemplateCall _ as i ->
     (i:>AstJs.instr)::inf

and fbloc b =
 let c=instr b [] in
 `Bloc c

and bloc b cpsInfo =
 List.fold_right instr b cpsInfo

and program p=
 bloc p []

let run=
 program
