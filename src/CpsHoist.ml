(*w
 * ====Hoisting of instructions====
 *
 * This pulls all the instructions marked  with ^^`Hoist^^ (defined in
 * [[AstCpsHoit.ml.html|AstCpsHoist]]). These instructions should be executed
 * before the expression they mark is evaluated, they should also be evaluated
 * when and only when the expression they mark is evaluated.
 *
 *)
module CpsMonad=
struct
 type 'a m = 'a * (AstCpsInt.instr list)
 let return a=(a,[])
 let bind (x,ctx) f =
  let res,ctx2=(f x) in
  res,(ctx@ctx2)
 let run (x,_) = x
end

module Conv=AstCpsInt.Trav.TranslateFrom(AstCpsHoistInt)(CpsMonad)
open Conv
module T=Conv.CloseRec(
 functor(Self:Translation)->
 struct
  module Super=Base(Self)
  include Super
  let expr=function
    (*w handles lazyness of the `And operator*)
   | `Binop(`And,e1,e2) ->
      let e1',ctx1=Self.expr e1
      and e2',ctx2=Self.expr e2
      in
      (match ctx2 with
        | [] ->  `Binop(`And,e1',e2'),ctx1
        | _ ->
           let id=TypeEnv.fresh ~hint:"hoistedAnd" () in
           `Ident id,ctx1@[`Var (id,Some e1');
                           `If(`Ident id,
                               `Bloc ctx2,
                               `Assign (`Ident id,`Cst (`Bool false))
                              )]
      )
   (*w Lazyness of the `Or operator*)
   | `Binop(`Or,e1,e2) ->
      let e1',ctx1=Self.expr e1
      and e2',ctx2=Self.expr e2
      in
      (match ctx2 with
        | [] ->  `Binop(`Or,e1',e2'),ctx1
        | _ ->
           let id=TypeEnv.fresh ~hint:"hoistedAnd" () in
           `Ident id,ctx1@[`Var (id,Some e1');
                           `If(`Unop (`Not ,`Ident id),`Bloc ctx2,`Bloc [])]
      )
   | `Hoist (e,i) ->
       let e',ctx=Self.expr e
       and i',_=Self.instr i in
       assert ((snd (Self.instr i))= []);
       e',(i'::ctx)
   | #In.expr as e -> Super.expr e

  let instr= function
   | `While(e,b) ->
      let e,ctx = Self.expr e
      and b,_ = Self.instr b in
      let _ = b,e,ctx in
      (match ctx with
        | [] -> `While(e,b),[]
        | _ -> `Bloc (ctx@[`While(e,`Bloc (b::ctx))]),[])
   | i ->
      (match Super.instr i with
        | i,[] -> i,[]
        | i,ctx -> `Bloc (ctx@[i]),[])
 end)

let run p=
 CpsMonad.run (T.program p)
