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

let specBase =
 ["-v",Arg.Unit version,"prints the version and exits"]

let (++) f g x= g (f x)

let opt=new Optimise.opt
 [DeadCode.pass;BranchMerger.pass;TailRec.pass;Hoisting.pass;Unbloc.pass]

let spec=opt#spec@specBase

let reportLoc l =
 eprintf "%s\n" (Pos.locToString l)

let parse file=
 with_open_in file begin
  fun c ->
   let lb = Lexing.from_channel c in
   try
    Lexer.setFile lb file;
    Parser.program Lexer.lex lb
   with
    | Lexical_error s ->
	   reportLoc (lexeme_start_p lb, lexeme_end_p lb);
	   eprintf "lexical error: %s\n@." s;
       exit 1
    | Parsing.Parse_error ->
	   reportLoc (lexeme_start_p lb, lexeme_end_p lb);
	   eprintf "syntax error in parse rule: \"%s\"\n@." !ParseInfo.currentRule;
       exit 1
 end

let process=
 parse
 ++CpsTrans.run
 ++CpsPropagate.run
 ++Cps.run
 ++opt#run
 ++EmitJs.print

let file =
 let file = ref None in
 let setFile s =
  file := Some s
 in
 Arg.parse spec setFile usage;
 match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let () =
 try
  process file
 with
  | e ->
	 eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	 exit 2
