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

let specs=["-v",Arg.Unit version,"prints the version and exits";
           "-tex-out",Arg.Unit Printer.setTexFormat,"pretty prints the trees as"
            ^ "tex code";
            "-stdin",Arg.Set readFromStdIn,"read data from standard in"
          ]

let (++) f g x= g (f x)


let parseError lexbuf s=
 eprintf "%s\n%s\n@." (Pos.locToString (lexeme_start_p lexbuf,lexeme_end_p lexbuf)) s;
 exit 1

let parse file=
 with_open_in file begin
  fun c ->
   let lb = Lexing.from_channel c in
   try
    Lexer.setFile lb file;
    Parser.program Lexer.token lb
   with
    | Lexical_error s ->
       parseError lb (sprintf "lexical error: %s" s)
    | Parsing.Parse_error ->
       parseError lb (sprintf "syntax error in parse rule: \"%s\"" !ParseInfo.currentRule);
 end

let process=
 parse++Cps.compile

let specs=Arg.align (Cps.specs@specs)

let () =
 let file = ref None in
 Arg.parse specs (fun s -> file := Some s) usage;
 match !file with
  |Some f ->begin
     try
      process f
     with e ->
	  eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	  exit 2
    end
  | None -> Arg.usage specs usage; exit 1
