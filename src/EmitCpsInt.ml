(*w
 * ====AstCpsInt pretty printer====
 *
 * This pretty printer is an extension of the one below it and is extensible...
 *)
open Printer

module Process(From:AstCpsInt.Trav.AstDef)=
struct
 module Conv=AstCpsInt.Trav.Conv(From)(From)(PrinterMonad)
 include Conv
 module Main(S:Translation):PartialTranslation=
 struct

  module Tmp=EmitBase.Process(From)
  module SuperIn=Tmp.In
  module Super=Tmp.Main(S)
  include Super
  include Convenience(struct include S module In=From end)
  let ty _ = assert false
  let cps i =
   kwd "cps:" ^^ i

  let expr=function
   | `CpsFun (al,b) -> cps (Super.expr (`Fun(al,b)))
   | #SuperIn.expr as e -> Super.expr e

  let cpsAff aff i =
   match aff with
    | None -> cps i
    | Some s -> bracket (bracket((S.ident s) ^^ (punct "=") ^^ i))

  let callCC=`Ident "CallCC"
  let instr i =
   let grp=ref true in
   let r=match i with
    | `Cps i -> cps (S.instr i)
    | `CpsCall (a,e,al) ->
       cpsAff a (instr (`Call (e,al)))
    | `CpsRet i -> Super.instr (`Ret i)
    | `CallCC (a,e,el) ->
       let args=join S.expr (e::el) (punct ",") in
       cpsAff a ((kwd "CallCC")^^ (par args))
    | `Throw (e1,e2) ->
       (kwd "throw")^^(par ((S.expr e1)^^(punct ",")^^(S.expr e2)))
    | #SuperIn.instr as i -> grp:=false;Super.instr i
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
