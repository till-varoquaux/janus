(*w
  ====AstCpsInt pretty printer====
*)
open Printer

module Process(From:AstCpsInt.Trav.AstDef)=
struct
 module Conv=AstCpsInt.Trav.Conv(From)(From)(PrinterMonad)
 include Conv
 module Main(S:Translation):PartialTranslation=
 struct

  module Tmp=EmitBase.Process(S.In)
  module Super=Tmp.Main(S)
  include Super
  include Convenience(S)
  let ty _ = assert false
  let cps i =
   kwd "cps:" ^^ i

  let expr=function
   | `CpsFun (al,b) -> cps (Super.expr (`Fun(al,b)))
   | #Old.expr as e -> Super.expr e

  let cpsAff aff i =
   match aff with
    | None -> cps i
    | Some s -> bracket (bracket((S.ident s) ^^ (punct "=") ^^ i))

  let instr i =
   let grp=ref true in
   let r=match i with
    | `Cps i -> cps (S.instr i)
    | `CpsCall (a,e,al) ->
       cpsAff a (instr (`Call (e,al)))
    | #Old.instr as i -> grp:=false;Super.instr i
       (*| `CallCC ident?,expr
       | `Throw expr,expr*)
    | `CallCC _ | `Throw _ -> assert false
   in
   if !grp then
    fgrp r
   else
    r
 end
end

module P=Process(AstCpsInt)
module rec D:P.Translation=P.Main(D)

let print (p:AstCpsInt.program):string=
 Printer.toString (D.program p)
