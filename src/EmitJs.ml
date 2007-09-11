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
 module SuperIn=Tmp.In
 module Super=Tmp.Main(S)
 include Super

 include Convenience(struct include S module In=AstJs end)

 let rec instrs=function
  | (`Var (i,None))::r -> squishVar [i] r
  | (i::t) -> (S.instr i)::(instrs t)
  | [] -> []
 and squishVar vars=function
  | (`Var (i,None))::r -> squishVar (i::vars) r
  |  l -> (kwd "var " ^^(join ident vars (punct ",")))::instrs l

 let bloc l =
  let il=instrs l in
  let r=braceIndent (sjoin il instrSep) in
  Ws.add blocs r;
  r

 (*w
   This is the only function we will need to override here. We will take
   advantage of the fact that we can break abstraction to do more precise
   pretty printing of If-Then-Else instructions
 *)
 let instr (i)=
  (*w
    Wether we will need to group the result in a fgrp
  *)
  let r=match i with
   | `Bloc b -> bloc b
   | `WithCtx (e,b,_) -> (kwd "with")^^(par (S.expr e )) ^^ (blocOrInstr b)
   | `Fundecl(i,args,b) ->
      let b=protectInstr b in
      (kwd "function ")^^(ident i)^^(par(join ident args (punct ",")))^^b
   | `If (e,b,`Bloc []) -> (kwd "if") ^^ (cond e) ^^ (blocOrInstr b)
   | `If (e,b1,(`If _ as b2)) ->
      (kwd "if") ^^ (cond e) ^^ (blocOrInstr ~breakAfter:true b1) ^^
       (kwd "else ")^^(S.instr b2)
   | `Labeled (lbl,i) -> (ident lbl) ^^(punct ":")^^(S.instr i)
   | `Continue (Some lbl) -> (kwd "continue") ^^ break ^^ (ident lbl)
   | `Continue None -> kwd "continue"
   | `Break (Some lbl) -> (kwd "break") ^^ break ^^ (ident lbl)
   | `Break None -> kwd "break"
   | #SuperIn.instr as i -> Super.instr i
  in
   grpInstr r

end

module D=Close(Main)

let print (p:AstJs.program):string=
 Printer.toString (D.program p)
