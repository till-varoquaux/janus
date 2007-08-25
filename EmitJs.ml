(*w
  ====AstJs pretty printer====
  This is where we will print a javascript document. We will emit an
  [[AstJs.ml.html|AstJs]] tree. This is an extension of the printer of AstBase
  trees.
*)
open Printer

module Conv=AstJs.Trav.TranslateFrom(AstJs)(PrinterMonad)
module Close=Conv.Make

module Main(S:Conv.Translation):Conv.PartialTranslation=
struct

 module Tmp=EmitBase.Process(AstJs)
 module Super=Tmp.Main(S)
 include Super

 include Convenience(struct include S module In=AstJs end)

 (*w
   This is the only function we will need to override here. We will take
   advantage of the fact that we can break abstraction to do more precise
   pretty printing of If-Then-Else instructions
 *)
 let instr (i)=
  (*w
    Wether we will need to group the result in a fgrp
  *)
  let grp=ref true in
  let r=match i with
   | `WithCtx (e,b,_) -> (kwd "with")^^(par (S.expr e )) ^^ (blocOrInstr b)
   | `Fundecl(i,args,b) ->
      let b=protectInstr b in
      (kwd "function ")^^(ident i)^^(par(join ident args (punct ",")))^^b
   | `If (e,b,`Bloc []) -> (kwd "if") ^^ (cond e) ^^ (blocOrInstr b)
   | `If (e,b1,(`If _ as b2)) ->
      (kwd "if") ^^ (cond e) ^^ (blocOrInstr ~breakAfter:true b1) ^^
       (kwd "else ")^^(S.instr b2)
   | `Labeled (lbl,i) -> (ident lbl) ^^(punct ":")^^(S.instr i)
   | `Continue lbl -> (kwd "continue") ^^ break ^^ (ident lbl)
   | `Break lbl -> (kwd "break") ^^ break ^^ (ident lbl)
   | #Conv.In.instr as i -> grp:=false;Super.instr i
  in
  if !grp then
   fgrp r
  else
   r
end

module D=Close(Main)

let print (p:AstJs.program):string=
 Printer.toString (D.program p)
