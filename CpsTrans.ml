(*w
  This pass does all the type checking and marking for cps translations.
*)
open Pos
open General
open AstStd
type ty'= AstCpsHoistInt.ty
type ident'= AstCpsHoistInt.ident
type instr'=AstCpsHoistInt.instr
type expr'=AstCpsHoistInt.expr
type program'=AstCpsHoistInt.program
type lvalue'=AstCpsHoistInt.lvalue
type ctx=instr' list

(*w
   This is the type of a compiled function call...
*)
type compiledCall={
 cps:bool;
 args:expr' list;
 body:expr'
}

let nullInstr=`Bloc []
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
  Translate a macro element. Literals are kept as they are but macroelements are
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
 List.iter l
  ~f:(fun {node=i;loc=p} ->
       if StringHashtbl.mem defList i then
        redef ~previousPos:(StringHashtbl.find defList i) p i
       else
        StringHashtbl.add defList ~key:i ~data:p)

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
 let args=List.map ~f:unPos args in
 let b=List.map ~f:(macroElem args) b in
 if cps then
  `CpsMacro(b,ty)
 else
  `Macro(b,ty)

let ident=TypeEnv.ident

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
 | `Fun (args,_) -> `Arrow ((List.map ~f:(fun _ -> `T) args),`T)
 | `Call (e,_) ->
    (match typeExpr env e with
      | `CpsArrow(_,b) -> b
      | `Arrow (_,b) -> b
      | `T -> `T)
 | `Ident i -> typeIdent env i
 | _ -> `T

and typeIdent env i =
 match TypeEnv.ty i env with
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
     List.iter tl ~f:(compatible `T);compatible `T ty
  | `Arrow (t1,t'1),`Arrow(t2,t'2)
  | `CpsArrow (t1,t'1),`CpsArrow(t2,t'2) ->
     (try
       List.iter2 ~f:compatible t1 t2
      with Invalid_argument _ ->
       error ("Type error:wrong arity")
     );
     compatible t'1 t'2
  | `CpsArrow _ , _ | _ , `CpsArrow _ ->
     error ("Cps functions can be used as arguments only when the type " ^
             "explicitly permits it")

let rec lvalue env:lvalue ->lvalue'=
 function
  | `Ident i -> `Ident (ident i env)
  | `ObjAccess (lv,i) ->
     let lv = lvalue env lv
     and i = ident i env in
     `ObjAccess (lv,i)
  | `ArrayAccess (lv,e) ->
     let lv = lvalue env lv
     and e = expr env e in
     `ArrayAccess (lv,e)

(*w
  Checks the args in a function call...
*)
and checkArg env al tyl=
 try
  List.iter2 ~f:(fun e ty -> compatible (typeExpr env e) ty ) al tyl
 with Invalid_argument _ ->
  error "Invalid argument count in call"

and args env al : (expr' list)=
 let arg a al=
  let a=expr env a in
  a::al
 in
 List.fold_right al ~f:arg ~init:[]

(*w
  Takes a function call and returns all the necessary informations to compil it.
*)
and callCompile env (`Call (e,al)) : compiledCall =
  let funTy= typeExpr env e in
 (match funTy with
   | `T -> ()
   | `Arrow (tl,_) | `CpsArrow (tl,_) -> checkArg env al tl
 );
 let al=args env al
 and e = expr env e in
 {cps=(match funTy with
        | `CpsArrow _ -> true
        | _ -> false);
  args=al;
  body=e
 }

and isMacro env=
 function
  | `Pos _ as p -> protect (isMacro env) p
  | `Ident i ->
     begin
      match TypeEnv.ty i env with
       | `Macro _ | `CpsMacro _ -> true
       | _ -> false
     end
  | _ -> false

(*w
  Converts an expression

  ^^eType^^ Is used to keep the type information.
*)
and expr ?(eType=(`T:ty)) env:expr -> expr'=function
 | `Pos _ as p -> protect (expr ~eType:eType env) p
 | `Typed (e,t) -> expr env e ~eType:t
 | `Obj (pl) ->
    let pl'=List.fold_right pl
     ~init:[]
     ~f:(fun ({node=i;loc=_},e) pl ->
          let e'=expr env e in
          ((i,e')::pl))
    in
    `Obj pl'
 | `Array (el) ->
    let elems=List.fold_right el
     ~init:[]
     ~f:(fun e el ->
          let e2 = expr env e in
          e2::el)
    in
    `Array (elems)
    (*TODO: Array typing*)
 | `ArrayAccess (e,idx) ->
    let e=expr env e
    and idx=expr env idx in
    `ArrayAccess (e,idx)
 | `ObjAccess (e,id) ->
    let e=expr env e in
    `ObjAccess (e,ident id env)
 | `Cst _ as c -> c
 | `CallCC (e,el) ->
    if not (TypeEnv.cps env) then
     error "Cannot use \"callcc\" in a non cps function.";
    let e'=expr env e
    and el'=List.map el ~f:(expr env) in
    let ret = TypeEnv.fresh ~hint:"AssignedVar" () in
    `Hoist(`Ident ret,`CallCC (Some ret,e',el'))
 | `Call _ as c ->
    let c = callCompile env c in
    if c.cps then begin
     if not (TypeEnv.cps env) then
      error "Cannot call a cps function in a non cps function.";
     let ret = TypeEnv.fresh ~hint:"AssignedVar" () in
     `Hoist(`Ident ret,`CpsCall(Some ret,c.body,c.args))
    end else
     `Call(c.body,c.args)
 | `BlockingEv (handler,args) ->
    if not (TypeEnv.cps env) then
     error "Cannot call a blocking event handler in a non cps function.";
    let ret = TypeEnv.fresh ~hint:"AssignedVar" () in
    let args=List.map args ~f:(expr env) in
    `Hoist(`Ident ret,`CpsCall(Some ret,expr env handler,args))
 | `Ident i ->
    (`Ident (ident i env))
 | `Fun (il,b) ->
    checkRedefs il;
    let env = ref (TypeEnv.oldify env) in
    let it,_,cps = (match eType with
                     | `CpsArrow (it,ret) -> (it,ret,true)
                     | `Arrow (it,ret) -> (it,ret,false)
                     | `T -> ((List.map ~f:(fun _ -> `T) il),`T,false)
                   )
    in
    List.iter2 il it
     ~f:(fun i ty -> env := TypeEnv.add i (ty:>ty') (!env))
    ;
    let env=TypeEnv.setCps cps !env in
    let b=fbloc env b
    and il =List.map ~f:(fun i -> ident i env) il
    in
    if cps then
     `CpsFun(il,b)
    else
     `Fun(il,b)
 | `Unop (u,e) ->
    let e=expr env e in
    `Unop (u,e)
 | `Binop (b,e1,e2) ->
    let e1=expr env e1
    and e2=expr env e2 in
    `Binop (b,e1,e2)
 | `Hoist (e,i) ->
    let i,env=instr env i in
    let e=expr env e in
    `Hoist (e,i)

and instr env : instr -> (instr'*TypeEnv.t)=
 function
  | `TemplateCall _-> assert false
  | `Pos _ as p -> protect (instr env) p
  | `Macro (i,_,_,_) | `CpsMacro (i,_,_,_) as m ->
     let m=typeMacro m in
     let env=TypeEnv.add i m env in
     nullInstr,env
  | `Var (i,None) ->
     `Var(ident i env,None),(TypeEnv.add i `T env)
  | `Var (i,Some e) ->
     let env=TypeEnv.add i ((typeExpr env e):>ty') env in
     let i=ident i env in
     let e=expr env e in
     `Var (i,Some e),env
  | `Assign (lv,e) ->
     let lv=lvalue env lv
     and e=expr env e in
     `Assign (lv,e),env
  | `Call (i,el) when isMacro env i ->
     let rec getId=
      (function
        | `Pos (_,p) -> getId p
        | `Ident i -> i
        | _ -> assert false
      )
     in
     ( match TypeEnv.ty (getId i) env with
        | `Macro (b,tyl) ->
           checkArg env el tyl;
           let args=args env el in
           `TemplateCall (args,b),env
        | `CpsMacro (b,tyl) ->
           checkArg env el tyl;
           let args=args env el in
           `CpsTemplateCall (args,b),env
        | _ -> assert false)
  | `Call _ as c ->
     let c = callCompile env c in
     let call=if c.cps then begin
      if not (TypeEnv.cps env) then
       error "Cannot call a cps function in a non cps function.";
      `CpsCall(None,c.body,c.args)
     end else
      `Call(c.body,c.args) in
     call,env
  | `BlockingEv (handler,args) ->
     if not (TypeEnv.cps env) then
      error "Cannot call a blocking event handler in a non cps function.";
     let args=List.map ~f:(expr env) args in
     `CpsCall(None,expr env handler,args),env
  | `If (e,b1,b2) ->
     let e=expr env e
     and b1,_=instr env b1 in
     let b2,_=instr env b2 in
     `If (e,b1,b2),env
  | `Bloc b ->
     let b',_ = bloc (TypeEnv.oldify env) b in
    `Bloc b',env
  | `Ret (Some e) ->
     let e=expr env e in
     let r = (
      if TypeEnv.cps env then
       `CpsRet (Some e)
      else
       `Ret (Some e)
     )in
     r,env
  | `Ret None ->
     let r = (
      if TypeEnv.cps env then
       `CpsRet None
      else
       `Ret None
     ) in
     r,env
     (*TODO: check return type*)
  | `Throw (e1,e2) ->
     if not (TypeEnv.cps env) then
      error "Cannot use \"throw\" in a non cps function.";
     let e1'=expr env e1
     and e2'=expr env e2 in
     `Throw (e1',e2'),env
      (*TODO: check for thrown value*)
  | `CallCC (e,el) ->
     if not (TypeEnv.cps env) then
      error "Cannot use \"callcc\" in a non cps function.";
     let e'=expr env e
     and el'=List.map ~f:(expr env) el in
     `CallCC (None,e',el'),env
  | `Abort ->
     if not (TypeEnv.cps env) then
      error "Cannot call a abort in a non cps function.";
     `Abort,env
  | `While (e,b) ->
     let e=expr env e
     and b,_=instr env b in
     `While (e,b),env
  | `Expr e ->
     `Expr (expr env e),env

and bloc (env:TypeEnv.t) : instr list -> (instr' list*TypeEnv.t) = function
 | [] -> [],env
 | h::t ->
    let i1,env=instr env h in
    let r2,env=bloc env t in
    i1::r2,env

and fbloc env b : instr' =
 let b,_=instr env b in
 b

let program (p:program):program'=
 fst (bloc TypeEnv.empty p)

let run=
 program
