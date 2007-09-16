(*w
 * ====Cps conversion 1/2====
 * This pass does all the type checking. In this pass types are erased,
 * variables are renamed to have a unique name. Since we remove types we Cps
 * calls are differenciated from normal function calls.
 *
 * **TODO** object and array typing, check return and callcc types.
 *
 * A solution could be to split this pass in three different passes:
 *
 * - variables are renamed to have a unique name.
 * - Types are erased.
 * - Change cps expressions to instruction contexts.
 *
 * **Bug**
 *
 *%%
 *var f=function(){};
 *for(var i=0;i<10;i++){
 *  var a=i;
 *  if (i==5)
 *   f=function(){print(a)};
 *};
 *f()
 *%%
 *
 * prints 9 (it should print 5)
 *
 * **Grade** F
 *)
open Pos
open General
module E=TypeEnv

(*w
 *This is the type of a compiled function call...
 *)
type compiledCall={
 cps:bool;
 args:AstCpsHoistInt.expr list;
 body:AstCpsHoistInt.expr
}

(*w
 * ==Scoping==
 *)
(*w
 * The error message to print when an identifier is defined twice
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
 * Checks wether no identifier is defined twice in the list given as argument.
 *)
let checkRedefs (l:AstStd.ident list)=
 let defList=StringHashtbl.create 17 in
 List.iter l
  ~f:(fun {node=i;loc=p} ->
       if StringHashtbl.mem defList i then
        redef ~previousPos:(StringHashtbl.find defList i) p i
       else
        StringHashtbl.add defList ~key:i ~data:p)

(*w
 * == Type checking ==
 *)

(*w
 * Returns the type of an expression in a given environement.
 * All the defined function have been marked during parsing. If this isn't the
 * case `T should be fine anyways
 *)
let rec typeExpr e env=
 match e with
  | `Pos _ as p -> protect (fun e -> typeExpr e env) p
  | `Typed (_,ty) -> ty
  | `Call (e,_) ->
     (match typeExpr e env with
       | `CpsArrow(_,b) -> b
       | `Arrow (_,b) -> b
       | `T -> `T)
  | `Ident i -> E.ty i env
  | _ -> `T
     (*
  | `ArrayAccess _ -> error "not handling arrays yet"
  | `ObjAccess _ -> error "not handling objects yet"
     *)

let typeLvalue lv env=
 typeExpr (AstStd.lval2expr lv) env

(*w
 * Checks that two types are compatible; raises an error if not. Since our
 * subtyping relation is purelly symetrical compatibility is also a symmetrical
 * relation.
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

(*w
 *   Checks the args in a function call...
 *)
and checkArg al tyl env=
 try
  List.iter2 ~f:(fun e ty -> compatible (typeExpr e env) ty ) al tyl
 with Invalid_argument _ ->
  error "Invalid argument count in call"

(*w
 * ==Pulling it all together==
 *)
module EnvMonad=
struct
 type 'a m= E.t -> ('a*E.t)
 let run f= fst (f E.empty)
 let bind (f:'a m) (g:'a -> 'b m) : 'b m=
  (fun x ->( let (a,_)= f x in
            (fst((g a) x)),x
          ))
 let return (a:'a) : 'a m=fun d -> (a,d)
end

module Conv=AstBase.Trav.Conv(AstStd)(AstCpsHoistInt)(EnvMonad)

module Process=Conv.CloseRec(
 functor(Self:Conv.Translation) ->
 struct
  module Super=Conv.Base(Self)
  include Super

  (*w
   * The following two function are upcast of functions we are inheriting from
   * the superclass.
   * This is required since upcasts are not implicit in OCaml.
   *)
  let sInstr i env=
   let i,env=Super.instr i env in
   (i :> AstCpsHoistInt.instr),env

  let sExpr e env=
   let e,env=Super.expr e env in
   (e :> AstCpsHoistInt.expr),env

  (*w
   * Function calls are common beetween expression and instructions. This
   * function compiles function call.
   * Takes a function call and returns all the necessary informations to compile
   * it.
   *)
  let callCompile env (`Call (e,al)) : compiledCall =
   let funTy= typeExpr e env in
   (match funTy with
     | `T -> ()
     | `Arrow (tl,_) | `CpsArrow (tl,_) -> checkArg al tl env
   );
   let al=List.map al ~f:(fun a -> fst(Self.expr a env))
   and e,_ = Self.expr e env in
   {cps=(match funTy with
          | `CpsArrow _ -> true
          | _ -> false);
    args=al;
    body=e}

  let ident id env=(E.ident id env),env

  let rec bloc b env=
   match b with
    | [] -> [],env
    | h::t ->
       let i1,env=Self.instr h env in
       let r2,env=bloc t env in
       i1::r2,env


  (*w
   * Converts an expression
   *
   * ^^eType^^ Is used to propagate the type information.
   *)
  let rec expr ?(eType=(`T:AstStd.ty)) (e:AstStd.expr) env=
   match e with
    | `Pos _ as p -> protect (fun e -> expr e ~eType:eType env) p
    | `Typed (e,t) -> expr e env ~eType:t
      (*Todo: typecheck CallCC and throw*)
    | `CallCC (e,el) ->
       if not (E.cps env) then
        error "Cannot use \"callcc\" in a non cps function.";
       let e',_=expr e env
       and el'=List.map el ~f:(fun e -> fst (expr e env)) in
       let ret = E.fresh ~hint:"AssignedVar" () in
       `Hoist(`Ident ret,`CallCC (Some ret,e',el')),env
    | `Call _ as c ->
      let c = callCompile env c in
      if c.cps then begin
       if not (E.cps env) then
        error "Cannot call a cps function in a non cps function.";
       let ret = E.fresh ~hint:"AssignedVar" () in
       `Hoist(`Ident ret,`CpsCall(Some ret,c.body,c.args)),env
      end else
       `Call(c.body,c.args),env
   | `BlockingEv (handler,args) ->
      if not (E.cps env) then
       error "Cannot call a blocking event handler in a non cps function.";
      let ret = E.fresh ~hint:"AssignedVar" ()
      and handler,_=expr handler env
      and args=List.map args ~f:(fun e -> fst (expr e env)) in
      `Hoist(`Ident ret,`CpsCall(Some ret,handler,args)),env
   | `Fun (args,b) ->
      checkRedefs args;
      let it,_,cps = (match eType with
                       | `CpsArrow (it,ret) -> (it,ret,true)
                       | `Arrow (it,ret) -> (it,ret,false)
                       | `T -> ((List.map ~f:(fun _ -> `T) args),`T,false))
      in
      let newEnv=List.fold_left2 args it
       ~f:(fun env argName argType -> E.add argName argType env)
       ~init:(E.setCps cps (E.oldify env)) in
      let b,_=Self.instr b newEnv
      and il =List.map ~f:(fun i -> fst(Self.ident i env)) args
      in
      if cps then
       `CpsFun(il,b),env
      else
       `Fun(il,b),env
   | `Hoist (e,i) ->
      let i,newEnv=Self.instr i env in
      let e,_=expr e newEnv in
      `Hoist (e,i),env
   | `Obj (pl) ->
      let pl'=List.fold_right pl
       ~init:[]
       ~f:(fun ({node=i;loc=_},e) pl ->
            let e',_=expr e env in
            ((i,e')::pl))
      in
      (`Obj pl'),env
   | #Conv.In.expr as e -> sExpr e env


  (*w
   * This erases the optional argument in expr defined above, it is required to
   * the module's type definition
   *)
  let expr e env = expr e env

  (*w
   * This function raises an error whenever it is called: it should never  be
   * called
   * The pass we are in erases type information. Types should never be
   * converted.
   *)
  let ty _ = assert false

  let instr i env=
   let expr e=fst (Self.expr e env) in
   match i with
    | `Pos _ as p -> protect (fun i -> Self.instr i env) p
    | `Var (id,a) as v ->
       let ty=(Option.map_default (fun e -> typeExpr e env) `T a) in
       let newEnv=E.add id ty env in
       sInstr v newEnv
    | `Assign (lv,e) as v ->
       compatible (typeLvalue lv env) (typeExpr e env);
       sInstr v env
    | `Call _ as c ->
       let c = callCompile env c in
       let call=if c.cps then begin
        if not (E.cps env) then
         error "Cannot call a cps function in a non cps function.";
        `CpsCall(None,c.body,c.args)
       end else
        `Call(c.body,c.args) in
       call,env
    | `BlockingEv (handler,args) ->
       if not (E.cps env) then
        error "Cannot call a blocking event handler in a non cps function.";
       let args=List.map ~f:expr args in
       `CpsCall(None,expr handler,args),env
    | `Bloc b ->
       let b',_ = bloc b (E.oldify env) in
       `Bloc b',env
    | `Ret v ->
       let v=Option.map expr v in
       let r = (
        if E.cps env then
         `CpsRet v
        else
         `Ret v
       )in
       r,env
     (*TODO: check return type*)
  | `Throw (e1,e2) ->
     if not (E.cps env) then
      error "Cannot use \"throw\" in a non cps function.";
     let e1'=expr e1
     and e2'=expr e2 in
     `Throw (e1',e2'),env
      (*TODO: check for thrown value*)
  | `CallCC (e,el) ->
     if not (TypeEnv.cps env) then
      error "Cannot use \"callcc\" in a non cps function.";
     let e'=expr e
     and el'=List.map ~f:expr el in
     `CallCC (None,e',el'),env
  | #Conv.In.instr as i -> (sInstr i env)

  let program = bloc
 end)

let run p = EnvMonad.run (Process.program p)
