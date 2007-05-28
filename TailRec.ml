(**
   Replaces tail reccursions with loops...
*)
let contlbl="$body"

(**
   This is an object we'll push on the stack to have a local context
*)
let jsCtxId = "$ctx"
let jsCtx=`Ident jsCtxId

(**
   Function id
*)
type fid= string*string list

type tail=
 | TailCall
 | Return
 | Instr

type pos=
 | Tail of fid
 | InFunc of fid
 | Main

let mergeTailInfo b1 b2=
 match b1,b2 with
  | TailCall ,_ | _,TailCall -> TailCall
  | Return,_ | _,Return -> Return
  | _ -> Instr

(**
   We want our navigation functions to take a position and return tailinfo
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

  let instr i pos =
   let newCtx=(`Assign (jsCtx,`EmptyCtx))
   and ctxAff a e =  `Assign (`ObjAccess  (jsCtx,a),e) in
   let tailCall al el=
    if al=[] then
     `Continue contlbl,TailCall
    else
     let aff=List.map2  ctxAff al el in
     `Bloc (newCtx::aff@[`Continue contlbl]),TailCall
   in
   match i,pos with
    | `Bloc b,pos -> let b',pos=bloc b pos in `Bloc b',pos
    | `If(e,b1,b2),pos ->
       let b1,ret1 = S.instr b1 pos
       and b2,ret2 = S.instr b2 pos
       in `If(e,b1,b2),(mergeTailInfo ret1 ret2)
    | `Ret (`Call (`Ident i,el)),(Tail (fname,args) | InFunc
     (fname,args)) when i=fname ->
      tailCall args el
    | (`Ret _ | `Return),_ -> i,Return
    | `Call (`Ident i,el),Tail (fname,args) when i=fname ->
       tailCall args el
    | `Fundecl (fname,args,body),_ ->
       let body,tail = S.instr body (Tail (fname,args)) in
       let body=
        if tail=TailCall then
         if args=[] then
          `Loop (contlbl,body)
         else
          let aff=List.map (fun i -> ctxAff i (`Ident i)) args in
          `Bloc ((`Var jsCtxId)::newCtx::aff@[`Loop (contlbl,(`WithCtx (jsCtx,body)))])
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
