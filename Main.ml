(*w
  ====Main====
  This is the entry point of the programm it reads command line options and
  contains the chain of all of the compiler passes.
*)
open Format
open Lexing
open Lexer
open Parser
open General

let usage = Printf.sprintf "usage: %s [options] file" (Filename.basename
                                                        Sys.executable_name)
let version ()=
 Printf.printf "%s, version: \"%s\"\n" Version.name Version.version;
 exit 0

let spec =
 ref ["-v",Arg.Unit version,"prints the version and exits"]

let (++) f g x= g (f x)

let opt=new Optimise.opt
 [
  DeadCode.pass;
  BranchMerger.pass;
  IdentFunDecls.pass;
  Hoisting.pass;
  TailRec.pass;
  Unbloc.pass
 ]


let parseError lexbuf s=
 eprintf "%s\n%s\n@." (Pos.locToString (lexeme_start_p lexbuf,lexeme_end_p lexbuf)) s;
 exit 1

let parse file=
 with_open_in file begin
  fun c ->
   let lb = Lexing.from_channel c in
   try
    Lexer.setFile lb file;
    Parser.program Lexer.lex lb
   with
    | Lexical_error s ->
       parseError lb (sprintf "lexical error: %s" s)
    | Parsing.Parse_error ->
       parseError lb (sprintf "syntax error in parse rule: \"%s\"" !ParseInfo.currentRule);
 end

let p trans printer name=
 let dump=ref false in
 spec:=("-dump-"^name,Arg.Set dump,"<undocumented>")::(!spec);
 fun p ->
  flush stdout;
  let x=trans p in
  if !dump then
   printf "%s\n==================\n" (printer x);
  flush stdout;
  x

let process=
 parse
 ++(p CpsTrans.run EmitCpsInt.print "cps")
 ++(p CpsPropagate.run EmitCpsInt.print "cpsprop")
 ++(p Cps.run EmitJs.print "rawjs")
 ++opt#run
 ++EmitJs.print

let spec=opt#spec@(!spec)

let () =
 let file = ref None in
 Arg.parse spec (fun s -> file := Some s) usage;
 match !file with
  |Some f ->begin
     try
      print_string (process f)
     with e ->
	  eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	  exit 2
    end
  | None -> Arg.usage spec usage; exit 1
