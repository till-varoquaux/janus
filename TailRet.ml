(*w
  This pass identify the empty return placed at the end of a function. Since
  functions call return implicitly when they end we migh as well remove them.
*)

module T=AstJs.Trav.Map(Monad.Id)

module D=T.Make(
 functor(S:T.Translation) ->
 struct
  module Super=T.Base(S)
  include Super

  let rec trimEndInstr lbl inFun=
   let self i=trimEndInstr lbl inFun i in
   let rec trimBloc=function
    | [] -> []
    | [i] -> [self i]
    | h::t-> (S.instr h)::(trimBloc t)
   in
   function
    | `Fundecl(name,args,body) ->
       let body=trimEndInstr None true body in
       `Fundecl(name,args,body)
    | `Ret None when inFun -> `Bloc []
    | `Bloc b -> `Bloc (trimBloc b)
    | `If (e,i1,i2) ->
       let e'=S.expr e
       and i1'=self i1
       and i2'=self i2
       in
       `If(e',i1',i2')
    | `WithCtx(e,i,ids) ->
       let e'=S.expr e
       and i'=self i in
       `WithCtx(e',i',ids)
    | `Labeled(lbl,`While(e,i)) ->
       let e'=S.expr e
       and i'=trimEndInstr (Some lbl) false i in
       `Labeled(lbl,`While(e',i'))
    | `Continue l when lbl=Some l -> `Bloc []
    | `Labeled(lbl,i) -> `Labeled(lbl, self i)
    | i -> S.instr i

  let instr = function
   | `Fundecl(name,args,body) ->
      let body=trimEndInstr None true body in
      `Fundecl(name,args,body)
   | `Labeled(lbl,`While(e,i)) ->
      let e'=S.expr e
      and i'=trimEndInstr (Some lbl) false i in
      `Labeled(lbl,`While(e',i'))
   | i -> Super.instr i

  let expr = function
   | `Fun (args,body) ->
      let body'=trimEndInstr None true body in
      `Fun(args,body')
   | e -> Super.expr e
 end
)

let pass:#Optimise.pass=
object
 method run = D.program
 method name="tailret"
 method description="Remove all the trailling empty \"return\" statements."
end
