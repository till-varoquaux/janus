module Id=Monad.Id

let rec unroll = function
 | [] -> []
 | (`Bloc b) :: l -> (unroll b) @ (unroll l)
 | i::l -> i::(unroll l)

let rec mergeHead b1 b2=
 match b1,b2 with
  | (h1::t1),(h2::t2) when h1=h2 ->
     let h,t = mergeHead t1 t2 in
     h1::h,t
  | t -> [],t

let mergeTail b1 b2=
 let rev=List.rev in
 let rtl,(rlhs,rrhs)=mergeHead (rev b1) (rev b2)
 in ((rev rlhs),(rev rrhs)),(rev rtl)

let merge b1 b2=
 let hd,(tl1,tl2)=mergeHead b1 b2 in
 let (r,l),tl=mergeTail tl1 tl2 in
 hd,(r,l),tl

module T=AstJs.Trav.Map(Id)

module D=T.Make(
 functor(S:T.T) ->
 struct
  module Super=T.Base(S)
  include Super

  let instr = function
   | `If(e,b1,b2) ->
      let hd,(r,l),tl= merge (unroll [b1]) (unroll [b2]) in
      if hd!=[] or tl!=[] then
       `Bloc (hd@(`If(e,(`Bloc r),(`Bloc l))::tl))
      else
       `If(e,(`Bloc r),(`Bloc l))
   | i -> Super.instr i
 end)

let pass:#Optimise.pass=
 object
  method run=D.program
  method name="branchmerge"
  method description="merging common parts beetween branches"
 end
