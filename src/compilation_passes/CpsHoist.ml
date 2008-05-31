(*w
 * ====Hoisting of instructions====
 *
 * This pulls all the instructions marked  with ^^`Hoist^^ (defined in
 * [[../AstCpsHoistInt.ml.html|AstCpsHoistInt]]). These instructions should be
 * executed before the expression they mark is evaluated, they should also be
 * evaluated when and only when the expression they mark is evaluated.
 *
 * **Grade** C
 *)

(*w
 * We decorate every returned value with the list of instructions we are
 * hoisting from it. This only really has a sense for expressions.
 *)
module HoistMonad=
struct
 type 'a m = 'a * (AstCpsInt.instr list)
 let return a = (a,[])
 let bind (x,ctx) f =
  let res,ctx2 = (f x) in
  res,(ctx@ctx2)
 let run = function
  | (x,[]) -> x
  | _ -> assert false
end

let return=HoistMonad.return

module Conv=AstCpsInt.Trav.TranslateFrom(AstCpsHoistInt)(HoistMonad)
include Conv.CloseRec(
 functor(Self:Conv.Translation)->
 struct
  module Super=Conv.Base(Self)
  include Super
  let expr=function
    (* handles lazyness of the `And operator*)
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
   (* Lazyness of the `Or operator*)
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
   | #Conv.In.expr as e -> Super.expr e

  let instr i =
   match i with
   | `While(e,b) ->
      let e,ctx = Self.expr e
      and b,_ = Self.instr b in
      (match ctx with
        | [] ->  return (`While(e,b))
        | _ -> return (`Bloc (ctx@[`While(e,`Bloc (b::ctx))])))
   | i ->
      (match Super.instr i with
        | i,[] -> return i
        | i,ctx -> return (`Bloc (ctx@[i])))
 end)

let run p=
 HoistMonad.run (program p)
