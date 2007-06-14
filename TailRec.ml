(*w
  ====Tail reccursion optimisation====
  In this pass we shall replace all tail reccursions with loops...
  In order to preserve closure we will use javascript's ^^with^^ statement.
*)
let contlbl="$body"

(*w
   This is an object we'll push on the stack to have a local context
*)
let jsCtxId = "$ctx"
let jsCtx=`Ident jsCtxId

(*w
  Function id's: the name of the function and of its arguments
*)
type fid= string*string list

(*w
  This is information regarding what was done in a block:

  - ^^TailCall^^ the block ends with a tail reccursion.
  - ^^Return^^ the block ends with a return instruction.
  - ^^Instr^^ the block ends with a normal instruction...
*)
type tail=
 | TailRec
 | Return
 | Instr

(*w
  This corresponds to the position in the code regarding tail reccursion:

  - ^^Tail fid^^:means we are in a tail reccursive position in the function "^^fid^^".
  - ^^InFunc fid^^ means we are currently in the function "^^fid^^".
  - ^^Main^^: we are in the main body of the programm.
*)
type pos=
 | Tail of fid
 | InFunc of fid
 | Main

(*w
  Returns the ^^tailinfo^^ for an instruction having two branches, ^^b1^^ and
  ^^b2^^ being their respectives ^^tailinfo^^s.
*)
let mergeTailInfo b1 b2=
 match b1,b2 with
  | TailRec ,_ | _,TailRec -> TailRec
  | Return,_ | _,Return -> Return
  | _ -> Instr

(*w
  This the monad we will use in our map module.
  We want our traversal functions to take a position and return tailinfo
*)
module TailMon=
struct
 type 'a m = pos -> 'a * tail
 let return a=fun _ -> (a,Instr)
 let bind (x:'a m) (f:'a -> 'b m): 'b m =
  fun b ->
   let b = match b with
    | Tail i -> InFunc i
    | b -> b
   in
   let v,b1 = x b in
   let r,b2  = f v b in
   let tail = mergeTailInfo b1 b2
   in
   r,tail
 let run x = fst (x Main)
end

module T=AstJs.Trav.Map(TailMon);;

module D=T.Make(
 functor(S:T.T) ->
 struct
  module Super=T.Base(S)
  include Super
  let bloc b pos=
   let rec aux= function
    | [] -> (match pos with Tail _ -> [`Return] | _ -> []),Return
    | [x] ->
       let i,tcall=S.instr x pos in(
        match i,pos,tcall with
         | _,Tail _,Instr -> [i;`Return],Return
         | _ -> [i],tcall)
    | x::l ->
       let i,_=(S.instr x Main)
       and l,tcall=aux l in
       i::l,tcall
   in
   let l,ret=aux b in
   l,ret

  let loop lbl i=
   `Labeled(lbl,`While ((`Cst (`Bool true)),i))

  let instr i pos =
   let tailRec (al:string list) (el:AstJs.expr list)=
    if al=[] then
     `Continue contlbl,TailRec
    else
     `Bloc[`Assign (jsCtx,`Obj (List.combine al el));`Continue contlbl],TailRec
   in
   match i,pos with
    | `Bloc b,pos -> let b',pos=bloc b pos in `Bloc b',pos
    | `If(e,b1,b2),pos ->
       let b1,ret1 = S.instr b1 pos
       and b2,ret2 = S.instr b2 pos
       in `If(e,b1,b2),(mergeTailInfo ret1 ret2)
    | `Ret (`Call (`Ident i,el)),
       (Tail (fname,args) | InFunc (fname,args)) when i=fname ->
       tailRec args el
    | (`Ret _ | `Return),_ -> i,Return
    | `Call (`Ident i,el),Tail (fname,args) when i=fname ->
       tailRec args el
    | `Fundecl (fname,args,body),_ ->
       let body,tail = S.instr body (Tail (fname,args)) in
       let body=
        if tail=TailRec then
         if args=[] then
          loop contlbl body
         else
          `Bloc [`Var jsCtxId;
                 `Assign (jsCtx,`Obj[]);
                 loop contlbl (`WithCtx (jsCtx,body,args))]
        else
         body
       in
       `Fundecl (fname,args,body),Instr
    | _ -> Super.instr i pos
 end
)

let pass:#Optimise.pass=
 object
  method run p =
   TailMon.run (D.program p)
  method name="tro"
  method description="tail recursion elemination"
 end
