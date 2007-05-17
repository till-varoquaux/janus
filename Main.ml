open Format
open Lexing
open Lexer
open Parser

let usage = Printf.sprintf "usage: %s [options] file" (Filename.basename
                                                        Sys.executable_name)
let specBase =
 []

let (++) f g x= g (f x)

let opt=new Optimise.opt
 [DeadCode.pass;BranchMerger.pass;TailRec.pass;Unbloc.pass]

let spec=opt#spec

let handle=
 CpsTrans.run
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

let reportLoc (b,e) =
 let l = b.pos_lnum in
 let fc = b.pos_cnum - b.pos_bol + 1 in
 let lc = e.pos_cnum - b.pos_bol + 1 in
 eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let p = Parser.program Lexer.lex lb in
    close_in c;
    handle p
  with
   | Lexical_error s ->
	  reportLoc (lexeme_start_p lb, lexeme_end_p lb);
	  eprintf "lexical error: %s\n@." s;
	  exit 1
   | Parsing.Parse_error ->
	  reportLoc (lexeme_start_p lb, lexeme_end_p lb);
	  eprintf "syntax error in parse rule: \"%s\"\n@." !ParseInfo.currentRule;
	  exit 1
   | e ->
	  eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	  exit 2
