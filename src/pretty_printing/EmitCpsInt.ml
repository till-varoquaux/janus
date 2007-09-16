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
 module Main(Self:Translation):PartialTranslation=
 struct

  module Tmp=EmitBase.Process(From)
  module SuperIn=Tmp.In
  module Super=Tmp.Main(Self)
  include Super
  include Convenience(struct include Self module In=From end)
  let ty _ = assert false
  let cps i = kwd "cps:" ^^ i

  let expr=function
   | `CpsFun (al,b) -> cps (Super.expr (`Fun(al,b)))
   | #SuperIn.expr as e -> Super.expr e

  let cpsAff aff i =
   match aff with
    | None -> cps i
    | Some s -> bracket (bracket((Self.ident s) ^^ (punct "=") ^^ i))

  let callCC=`Ident "CallCC"
  let instr i =
   let grp=ref true in
   let r=match i with
    | `Cps i -> cps (Self.instr i)
    | `CpsCall (a,e,al) ->
       cpsAff a (instr (`Call (e,al)))
    | `CpsRet i -> Super.instr (`Ret i)
    | `CallCC (a,e,el) ->
       let args=join Self.expr (e::el) (punct ",") in
       cpsAff a ((kwd "CallCC")^^ (par args))
    | `Throw (e1,e2) ->
       (kwd "throw")^^(par ((Self.expr e1)^^(punct ",")^^(Self.expr e2)))
    | #SuperIn.instr as i -> grp:=false;Super.instr i
   in
   if !grp then
    fgrp r
   else
    r
 end
end

let print (p:AstCpsInt.program):string=
 let module P=Process(AstCpsInt) in
 let module D=P.CloseRec(P.Main) in
 Printer.toString (D.program p)
