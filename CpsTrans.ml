open General
open AstStd
type ident'= AstCpsInt.ident
type instr'=AstCpsInt.instr
type expr'=AstCpsInt.expr
type program'=AstCpsInt.program
type ctx=instr' list

exception Fundecl of (bool*(ident' list)*instr')
 (*Used to move function declarations out of expressions...
   Should be done in another pass.
 *)

(**
   This is the type of a compiled function call...
*)
type compiledCall={
 cps:bool;
 ctx:ctx;
 args:expr' list;
 body:expr'
}

let redef l i=
 error ~pos:l (Printf.sprintf "cannot redefine \"%s\"" i)

(**
   Translate a macro element
*)
let macroElem al = function
| `Ident i ->
   (try
     `Ident (List.scan i al)
    with Not_found ->
     error (Printf.sprintf "Undefined ident \"%s\"" i)
   )
| `Literal _ as l -> l

let cont=dummyId "cont"

let typeMacro m=
 let cps,{node=i;loc=l},args,b,ty=
  match m with
   | (`Macro(n,args,b,ty)) -> false,n,args,b,ty
   | (`CpsMacro(n,args,b,ty)) -> true,n,args,b,ty
 in
 let checkArg {node=i;loc=p} l  =
  if List.mem i l then
   redef p i
  else
   i::l
 in
 let args=
  if cps then
   cont::args
  else
   args
 in
 let args=List.fold_right checkArg args [] in
 let b=List.map (macroElem args) b in
 if cps then
  `CpsMacro(b,ty)
 else
  `Macro(b,ty)

let ident=Env.ident

(**
   Returns the type of an expression in a given environement
**)
let rec typeExpr env=function
 | `Pos _ as p -> protect (typeExpr env) p
 | `Typed (_,ty) -> ty
 | `Fun _ -> assert false (*All defined function should be typed*)
 | `Call (e,_) ->
    (match typeExpr env e with
      | `CpsArrow(_,b) -> b
      | `Arrow (_,b) -> b
      | `T -> `T)
 | `Lval lv -> typeLvalue env lv
 | _ -> `T
and typeLvalue env=function
 | `Ident i ->
    (match Env.ty i env with
      | `Macro _ | `CpsMacro _ -> error "Cannot call a macro in an expression"
      | `Arrow _ | `CpsArrow _ | `T as t-> t
    )
 | `Array _ -> error "not handling arrays yet"
 | `Access _ -> error "not handling objects yet"

(**
   Checks that two types are compatible; raises an error if not.
**)
let rec compatible t1 t2=
 match (t1,t2) with
  | `T,`T -> ()
  | `Arrow (tl,ty),`T | `T,`Arrow (tl,ty) ->
     List.iter (compatible `T) tl;compatible `T ty
  | `Arrow (t1,t'1),`Arrow(t2,t'2)
  | `CpsArrow (t1,t'1),`CpsArrow(t2,t'2) ->
     (try
       List.iter2 compatible t1 t2
      with Invalid_argument _ ->
       error ("Type error:wrong arity")
     );
     compatible t'1 t'2
  | `CpsArrow _ , _ | _ , `CpsArrow _ ->
     error ("Cps functions can be used as arguments only when the type " ^
             "explicitly permits it")

let rec lvalue env=
 function
  | `Ident i -> `Ident (ident i env),[]
  | `Access (lv,i) ->
     let lv,ctx = lvalue env lv
     and i = ident i env in
     (`Access (lv,i)),ctx
  | `Array (lv,e) ->
     let lv,ctx1 = lvalue env lv
     and e,ctx2 = expr env e in
     (`Array (lv,e)),(ctx1@ctx2)

and checkArg env al tyl=
 try
  List.iter2 (fun e ty -> compatible (typeExpr env e) ty ) al tyl
 with Invalid_argument _ ->
  error "Invalid argument count in call"

and args env al : ((expr' list)*ctx)=
 let arg a (al,ctx)=
  let a,ctx2=expr env a in
  (a::al),(ctx@ctx2)
 in
 List.fold_right arg al ([],[])

(**
   Takes a function call and returns all the necessary informations to compil it
   .
*)
and callCompile env (`Call (e,al)) : compiledCall =
  let funTy= typeExpr env e in
 (match funTy with
   | `T -> ()
   | `Arrow (tl,_) | `CpsArrow (tl,_) -> checkArg env al tl
 );
 let al,ctx=args env al
 and e,ctx2 = expr env e in
 let ctx=ctx@ctx2
 in
 {cps=(match funTy with
        | `CpsArrow _ -> true
        | _ -> false);
  ctx=ctx;
  args=al;
  body=e
 }

and isMacro env=
 function
  | `Pos _ as p -> protect (isMacro env) p
  | `Lval (`Ident i) ->
     begin
      match Env.ty i env with
       | `Macro _ | `CpsMacro _ -> true
       | _ -> false
     end
  | _ -> false

(**
   Converts an expression

   @param inVdecl Are we already already in a var declaration
   (TODO: this is used to transform fundeclaration expressions to function
   declaration instructions. It should probably be done in another pass.)
   @param eType
*)
and expr ?(inVdecl=false) ?(eType=(`T:ty)) env:expr -> (expr'*ctx)=function
 | `Pos _ as p -> protect (expr ~inVdecl:inVdecl ~eType:eType env) p
 | `Typed (e,t) -> expr env e  ~inVdecl:inVdecl ~eType:t
 | `Cst _ as c -> c,[]
 | `Call _ as c ->
    let c = callCompile env c in
    if c.cps then
     let ret = Env.fresh ~hint:"AssignedVar" () in
     `Lval (`Ident ret) ,c.ctx@[`CpsCall(Some ret,c.body,c.args)]
    else
     `Call(c.body,c.args),c.ctx
 | `Lval lv ->
    let lv,ctx = lvalue env lv in
    (`Lval lv),ctx
 | `Fun (il,b) ->
    let env = ref (Env.oldify env) in
    let it,ret,cps = (match eType with
                   | `T -> assert false
                   | `CpsArrow (it,ret) -> (it,ret,true)
                   | `Arrow (it,ret) -> (it,ret,false)
                 )
    in
    List.iter2 (fun i ty -> env := Env.add i (ty:>ty') (!env)) il it;
    let env=Env.setCps cps !env in
    let b=fbloc env b
    and il =List.map (fun i -> ident i env) il
    in
    if (inVdecl) then(
     raise (Fundecl (cps,il,b))
    )else(
     let a = Env.fresh ~hint:"f" ()
     in (`Lval (`Ident a)),[
      if cps then
       `Cps(`Fundecl(a,il,b))
      else
       `Fundecl(a,il,b)
     ]
    )
 | `Unop _ -> assert false
 | `Binop (b,e1,e2) ->
    let e1,ctx1=expr env e1
    and e2,ctx2=expr env e2 in
    (`Binop (b,e1,e2)),(ctx1@ctx2)
     (*TODO handle lazyness in and and or*)
and instr env : instr -> (instr' list*Env.t)=
 function
  | `Fundecl _ | `TemplateCall _-> assert false
  | `Pos _ as p -> protect (instr env) p
  | `Macro (i,_,_,_) | `CpsMacro (i,_,_,_) as m ->
     let m=typeMacro m in
     let env=Env.add i m env in
     [],env
  | `Var (i,e) ->
     let env=Env.add i ((typeExpr env e):>ty') env in
     let i=ident i env in
     (try(
       let e,ctx=expr ~inVdecl:true env e in
       (ctx@[`Var (i,e)]),env
      ) with
       | Fundecl (true,il,b) -> [`Cps(`Fundecl (i,il,b))],env
       | Fundecl (false,il,b) -> [`Fundecl (i,il,b)],env
     )
  | `Assign (lv,e) ->
     let lv,ctx1=lvalue env lv
     and e,ctx2=expr env e in
     (ctx1@ctx2@[`Assign (lv,e)]),env
  | `Call (i,el) when isMacro env i ->
     let rec getId=
      (function
        | `Pos (_,p) -> getId p
        | `Lval (`Ident i) -> i
        | _ -> assert false
      )
     in
     ( match Env.ty (getId i) env with
        | `Macro (b,tyl) ->
           checkArg env el tyl;
           let args,ctx=args env el in
           (ctx@[`TemplateCall (args,b)]),env
        | `CpsMacro (b,tyl) ->
           checkArg env el tyl;
           let args,ctx=args env el in
           (ctx@[`Cps(`TemplateCall (args,b))]),env
        | _ -> assert false)
  | `Call _ as c ->
     let c = callCompile env c in
     let call=if c.cps then
      `CpsCall(None,c.body,c.args)
     else
      `Call(c.body,c.args) in
     (c.ctx@[call]),env
  | `If (e,b1,b2) ->
     let e,ctx=expr env e
     and b1,env=instr env b1 in
     let b2,env=instr env b2 in
     (ctx@[`If (e,`Bloc b1,`Bloc b2)]),env
  | `Bloc b -> let b',env' = bloc env b in
    b',env'
  | `Ret e ->
     let e,ctx=expr env e in
     let r = (
      if Env.cps env then
       `Cps(`Ret e)
      else
       `Ret e
     )in
     (ctx@[r],env)
     (*TODO: check return type*)
  | `While (e,b) ->
     let e,ctx=expr env e
     and b,env=instr env b in
     (ctx@[`While (e,(`Bloc b))]),env

and bloc (env:Env.t) : instr list -> (instr' list*Env.t) = function
 | [] -> [],env
 | h::t ->
    let r1,env=instr env h in
   let r2,env=bloc env t in
   (r1@r2),env

and fbloc env b : instr' =
 let b,_=instr env b in
 `Bloc b

let program (p:program):program'=
 fst (bloc Env.empty p)

let run=
 program
