(*w
 * ====Tail reccursion optimisation====
 * In this pass we shall replace all tail reccursions with loops...
 * In order to preserve closure we will use javascript's ^^with^^ statement.
 *
 * Tail reccursive functions are compiled to a function whose main body is
 * enclosed in a loop. A tail reccursive call is a jump to the begining of the
 * loop. In order to assure contexts are correctly handled we use a trick:
 * we build our contextes and then push them on the stack using ^^with^^.
 *
 * **Grade** D
 *)

let contlbl="$body"

(*w
 * This is an object we'll push on the stack to have a local context
 *)
let jsCtxId = "$ctx"
let jsCtx=`Ident jsCtxId

(*w
 * Function id's: the name of the function and of its arguments
 *)
type pos= (string*string list) option

(*w
 * This the monad we will use in our map module.
 * We want our traversal functions to take a position and return a node
 * decorated with a blocInfo
 *)
module TailMon=
struct
 type 'a m = pos -> 'a * bool
 let return a=fun _ -> (a,false)
 let bind (x:'a m) (f:'a -> 'b m): 'b m =
  fun b ->
   let v,b1 = x b in
   let r,b2  = f v b in
   r,b1||b2
 let run x = fst (x None)
end

module T=AstJs.Trav.Map(TailMon);;

module D=T.Make(
 functor(S:T.Translation) ->
 struct
  module Super=T.Base(S)
  include Super

  let loop lbl i=
   `Labeled(lbl,`While ((`Cst (`Bool true)),i))

  let instr i pos =
   let tailRec (al:string list) (el:AstJs.expr list)=
    if al=[] then
     `Continue (Some contlbl),true
    else
     `Bloc[`Assign (jsCtx,`Obj (List.combine al el));`Continue (Some contlbl)],true
   in
   match i,pos with
    | `If(e,b1,b2),pos ->
       let b1,ret1 = S.instr b1 pos
       and b2,ret2 = S.instr b2 pos
       in `If(e,b1,b2),(ret1||ret2)
    | `Ret (Some `Call (`Ident i,el)),Some (fname,args) when i=fname ->
       tailRec args el
    | `Call (`Ident i,el),Some (fname,args) when i=fname ->
       tailRec args el
    | `Fundecl (fname,args,body),_ ->
       let body,tailRec = S.instr body (Some (fname,args)) in
       let body=
        if tailRec then
         if args=[] then
          loop contlbl (`Bloc [body;`Ret None])
         else
          `Bloc [`Var (jsCtxId,Some (`Obj[]));
                 loop contlbl (`WithCtx (jsCtx,(`Bloc [body;`Ret None]),args))]
        else
         body
       in
       `Fundecl (fname,args,body),false
    | _ -> Super.instr i pos

 (*function used as expressions are anonymous, they cannot be directly reccursive*)
 end
)

let pass:#Optimise.pass=
 object
  method run p = TailMon.run (D.program p)
  method name="tro"
  method description="tail recursion elemination"
 end
