(*w
 * ====AstJs pretty printer====
 * This is where we will print a javascript document. We will emit an
 * [[AstJs.ml.html|AstJs]] tree. This is an extension of the printer of AstBase
 * trees.
 *)
open Printer

module Conv=AstJs.Trav.TranslateFrom(AstJs)(PrinterMonad)
module Main(Self:Conv.Translation):Conv.PartialTranslation=
struct

 module Tmp=EmitBase.Process(AstJs)
 module SuperIn=Tmp.In
 module Super=Tmp.Main(Self)
 include Super

 include Convenience(struct include Self module In=AstJs end)


 (*w
  * This function iterates over an instruction list to "squish" together all
  * subsequent variable definition. Instead of printing
  * %%
  * var a;
  * var b;
  * var c;
  * %%
  * it will print:
  * %%
  * var a,b,c;
  * %%
  *)
 let rec instrs=function
  | (`Var (i,None))::r -> squishVar [i] r
  | (i::t) -> (Self.instr i)::(instrs t)
  | [] -> []
 and squishVar vars=function
  | (`Var (i,None))::r -> squishVar (i::vars) r
  |  l -> (kwd "var"++(mapConcat vars ~f:ident ~sep:(punct ",")))::instrs l

 let bloc l =
  let il=instrs l in
  let r=braceIndent (concat il ~sep:instrSep) in
  Ws.add blocs r;
  r

 (*w
  * This is the only function we will need to override here. We will take
  * advantage of the fact that we can break abstraction to do more precise
  * pretty printing of If-Then-Else instructions
  *)
 let instr (i)=
  let r=match i with
   | `Bloc b -> bloc b
   | `WithCtx (e,b,_) -> (kwd "with")^^(par (Self.expr e )) ^^ (blocOrInstr b)
   | `Fundecl(i,args,b) ->
      let b=protectInstr b in
      (kwd "function")++(ident i)^^
       (par(mapConcat args ~f:ident ~sep:(punct ",")))^^b
   | `If (e,b,`Bloc []) -> (kwd "if") ^^ (cond e) ^^ (blocOrInstr b)
   | `If (e,b1,(`If _ as b2)) ->
      (kwd "if") ^^ (cond e) ^^ (blocOrInstr ~breakAfter:true b1) ^^
       (kwd "else")++(Self.instr b2)
   | `Labeled (lbl,i) -> (ident lbl) ^^(punct ":")^^(Self.instr i)
   | `Continue (Some lbl) -> (kwd "continue") ^^ break ^^ (ident lbl)
   | `Continue None -> kwd "continue"
   | `Break (Some lbl) -> (kwd "break") ^^ break ^^ (ident lbl)
   | `Break None -> kwd "break"
   | #SuperIn.instr as i -> Super.instr i
  in
   grpInstr r

end

module D=Conv.CloseRec(Main)

let print (p:AstJs.program):string=
 Printer.toString (D.program p)
