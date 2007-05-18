open AstCpsInt

type env={
 cont:AstJs.instr list;
 head:AstJs.instr list;
 vdecl:AstJs.ident list
}

let emptyEnv={
 cont=[];
 head=[];
 vdecl=[]
}

let return="$return"

let cpsCall inf affect=
 let a= (match affect with
          | None -> []
          | Some i -> [i])
 and cont=Env.fresh ~hint:"CpsCall" () in
 {inf with
   vdecl=cont::inf.vdecl;
   head=`Fundecl (cont,a,`Bloc (inf.cont))::inf.head;
 },cont

let rec instr (i:AstCpsInt.instr) inf=
 match i with
  | `Bloc b -> bloc b inf
  | `CpsTemplateCall (el,b) ->
     let inf,cont=cpsCall inf None in
     {inf with
       cont=[`TemplateCall (`Lval (`Ident cont)::el,b)]
     }
  | `CpsCall (a,e,el) ->
     let inf,cont=cpsCall inf a in
     {inf with
       cont=[`Call (e,`Lval (`Ident cont)::el)]
     }
  | `Cpsdecl (i,al,b) ->
     {inf with
       head=`Fundecl (i,(return::al),fbloc b)::inf.head;
       vdecl=i::inf.vdecl
     }
  | `Fundecl (i,al,b) ->
     {inf with
       head=`Fundecl(i,al,fbloc b)::inf.head;
       vdecl=i::inf.vdecl
     }
  | `Vardecl (i,e) ->
     {inf with
       cont=(`Assign((`Ident i),e))::inf.cont;
       vdecl=i::inf.vdecl
     }
      (*optimize me this...*)
  | `If (e,b1,b2) (*when (Env.cps inf.env)*)  ->
     let k=Env.fresh ~hint:"Ite" () in
     let inf={(*inf with*)
      head=(`Fundecl(k,[],`Bloc (inf.cont)))::inf.head;
               cont=([`Call(`Lval(`Ident k),[])]);
               vdecl=k::inf.vdecl
             }
     in
     let e1=instr b1 inf in
     let e2=instr b2 {e1 with cont=inf.cont} in
     {e2 with cont=[`If (e,(`Bloc e1.cont),(`Bloc e2.cont))]}
  | `While (e,i) (*when (Env.cps inf.env)*) ->
     let k=Env.fresh ~hint:"While" () in
     let cont=[`Call ((`Lval(`Ident k)),[])] in
     let i=instr i {inf with cont=cont} in
     {(*i with*)
      vdecl=k::i.vdecl;
      head=(`Fundecl (k,[],`Bloc [`If(e,(`Bloc i.cont),(`Bloc inf.cont))]))::i.head;
      cont=cont
     }
  | `CpsRet e ->
     {inf with cont=(`Call ((`Lval(`Ident return)),[e]))::inf.cont}
  | `Assign _ | `Call _ | `Vdecl _ | `Ret _ | `TemplateCall _ as i ->
     {inf with cont=(i:>AstJs.instr)::inf.cont}

and fbloc b =
 let c=instr b emptyEnv in
 `Bloc ((`Vdecl c.vdecl)::(c.head)@(c.cont))

and bloc b (cpsInfo:env) : env=
 List.fold_right instr b cpsInfo

and program p=
 let c=bloc p emptyEnv in
 (`Vdecl c.vdecl)::(c.head)@(c.cont)

let run=
 program
