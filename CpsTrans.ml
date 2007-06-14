(*w
  This pass does all the type checking and marking for cps translations.

  TODO: this pass is a bloody mess!!! it really needs some cleaning up.
  Hoisting of Cps Calls should really be made elsewhere
*)
open Pos
open General
open AstStd
type ty'= AstCpsInt.ty
type ident'= AstCpsInt.ident
type instr'=AstCpsInt.instr
type expr'=AstCpsInt.expr
type program'=AstCpsInt.program
type lvalue'=AstCpsInt.lvalue
type ctx=instr' list

(*w
   This is the type of a compiled function call...
*)
type compiledCall={
 cps:bool;
 ctx:ctx;
 args:expr' list;
 body:expr'
}

(*w
  The error message to print when an identifier is defined twice
*)
let redef ?previousPos pos id =
 match previousPos with
  | None ->error ~pos:pos (Printf.sprintf "cannot redefine \"%s\"" id)
  | Some p ->
     let previous=Pos.locToString p in
     let msg=Printf.sprintf
      "cannot redefine \"%s\" which was previously defined at %s" id previous
     in
     error ~pos:pos msg

(*w
  Translate a macro element. Literals are kept as they are but macroelemnts are
  converted to De Bruijn indices.
*)
let macroElem al = function
 | `Ident {node=i;loc=p} ->
    (try
      `Ident (List.scan i al)
     with Not_found ->
      error ~pos:p (Printf.sprintf "Undefined ident \"%s\"" i)
    )
 | `Literal _ as l -> l

let cont=dummyId "cont"

(*w
  Checks wether no identifier is defined twice in the list given as an argument.
*)
let checkRedefs (l:ident list)=
 let defList=StringHashtbl.create 17 in
 List.iter
  begin
   fun {node=i;loc=p} ->
    if StringHashtbl.mem defList i then
     redef ~previousPos:(StringHashtbl.find defList i) p i
    else
     StringHashtbl.add defList i p
  end l

(*w
  This typechecks a macro: it verifies none of the macros arguments have the
  same name and returns the macro type.

  The macro type is actually the macro itself, the arguments have been converted
  to de Bruijn's notation. It also contains the macro return type.

  TODO: The env should contains both types and macros in two separate tables...

  TODO: CPSMacros should take their continuation as an explicit parameter
*)
let typeMacro m =
 let cps,args,b,ty=
  match m with
   | (`Macro(_,args,b,ty)) -> false,args,b,ty
   | (`CpsMacro(_,args,b,ty)) -> true,args,b,ty
 in
 let args=
  if cps then
   cont::args
  else
   args
 in
 checkRedefs args;
 let args=List.map unPos args in
 let b=List.map (macroElem args) b in
 if cps then
  `CpsMacro(b,ty)
 else
  `Macro(b,ty)

let ident=Env.ident

(*w
   Returns the type of an expression in a given environement
*)
let rec typeExpr env=function
 | `Pos _ as p -> protect (typeExpr env) p
 | `Typed (_,ty) -> ty
    (*w
      All defined function should be typed we should therefor have passed though a
      ^^Typed^^ node before reaching this point.
    *)
 | `Fun _ -> assert false
 | `Call (e,_) ->
    (match typeExpr env e with
      | `CpsArrow(_,b) -> b
      | `Arrow (_,b) -> b
      | `T -> `T)
 | `Ident i -> typeIdent env i
 | _ -> `T

and typeIdent env i =
 match Env.ty i env with
  | `Macro _ | `CpsMacro _ -> error "Cannot call a macro in an expression"
  | `Arrow _ | `CpsArrow _ | `T as t-> t

and typeLvalue env=function
 | `Ident i -> typeIdent env i
 | `Array _ -> error "not handling arrays yet"
 | `Access _ -> error "not handling objects yet"

(*w
   Checks that two types are compatible; raises an error if not.
*)
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

let rec lvalue env:lvalue ->lvalue'*ctx=
 function
  | `Ident i -> `Ident (ident i env),[]
  | `ObjAccess (lv,i) ->
     let lv,ctx = lvalue env lv
     and i = ident i env in
     (`ObjAccess (lv,i)),ctx
  | `ArrayAccess (lv,e) ->
     let lv,ctx1 = lvalue env lv
     and e,ctx2 = expr env e in
     (`ArrayAccess (lv,e)),(ctx1@ctx2)

(*w
  Checks the args in a function call...
*)
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

(*w
  Takes a function call and returns all the necessary informations to compil it.
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
  | `Ident i ->
     begin
      match Env.ty i env with
       | `Macro _ | `CpsMacro _ -> true
       | _ -> false
     end
  | _ -> false

(*w
  Converts an expression

  ^^eType^^ Is used to keep the type information.
*)
and expr ?(eType=(`T:ty)) env:expr -> (expr'*ctx)=function
 | `Pos _ as p -> protect (expr ~eType:eType env) p
 | `Typed (e,t) -> expr env e ~eType:t
 | `Obj (pl) ->
    let pl',ctx=List.fold_left begin
     fun (pl,ctx) ({node=i;loc=_},e) ->
      let e',ctx'=expr env e in
      ((i,e')::pl),(ctx'@ctx)
    end  ([],[]) pl in
    `Obj pl',ctx
 | `Array (el) ->
    let elems,ctx=List.fold_right begin
     fun e (el,ctx) ->
      let e2,ctx2 = expr env e in
      (e2::el),(ctx2@ctx)
    end el ([],[]) in
    `Array (elems),ctx
    (*TODO: Array typing*)
 | `ArrayAccess (e,idx) ->
    let e,ctx=expr env e
    and idx,ctx2=expr env idx in
    `ArrayAccess (e,idx),(ctx@ctx2)
 | `ObjAccess (e,id) ->
    let e,ctx=expr env e in
    `ObjAccess (e,ident id env),ctx
 | `Cst _ as c -> c,[]
 | `Call _ as c ->
    let c = callCompile env c in
    if c.cps then
     let ret = Env.fresh ~hint:"AssignedVar" () in
     `Ident ret,c.ctx@[`Cps (`CpsCall(Some ret,c.body,c.args))]
    else
     `Call(c.body,c.args),c.ctx
 | `Ident i ->
    (`Ident (ident i env)),[]
 | `Fun (il,b) ->
    checkRedefs il;
    let env = ref (Env.oldify env) in
    let it,_,cps = (match eType with
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
    if cps then
     `CpsFun(il,b),[]
    else
     `Fun(il,b),[]
 | `Unop (u,e) ->
    let e,ctx=expr env e in
    `Unop (u,e),ctx
 | `Binop (b,e1,e2) ->
    let e1,ctx1=expr env e1
    and e2,ctx2=expr env e2 in
    (`Binop (b,e1,e2)),(ctx1@ctx2)
     (*TODO handle lazyness in and and or*)
and instr env : instr -> (instr' list*Env.t)=
 function
  | `TemplateCall _-> assert false
  | `Pos _ as p -> protect (instr env) p
  | `Macro (i,_,_,_) | `CpsMacro (i,_,_,_) as m ->
     let m=typeMacro m in
     let env=Env.add i m env in
     [],env
  | `Var (i,e) ->
     let env=Env.add i ((typeExpr env e):>ty') env in
     let i=ident i env in
     let e,ctx=expr env e in
     (ctx@[`Var (i,e)]),env
  | `Assign (lv,e) ->
     let lv,ctx1=lvalue env lv
     and e,ctx2=expr env e in
     (ctx1@ctx2@[`Assign (lv,e)]),env
  | `Call (i,el) when isMacro env i ->
     let rec getId=
      (function
        | `Pos (_,p) -> getId p
        | `Ident i -> i
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
      `Cps(`CpsCall(None,c.body,c.args))
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
     ctx@[r],env
     (*TODO: check return type*)
  | `Throw (e1,e2) ->
     let e1',ctx1=expr env e1
     and e2',ctx2=expr env e2 in
     ctx1@ctx2@[`Cps(`Throw (e1',e2'))],env
      (*TODO: check for thrown value and that we are in a CPS env*)
  | `CallCC e ->
     let e',ctx=expr env e in
     ctx@[`CallCC (None,e')],env
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
