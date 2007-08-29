(*w
  ====Main====
  This is the entry point of the programm it reads command line options and
  contains the chain of all of the compiler passes.
*)
open General

let usage = Printf.sprintf "usage: %s [options] file" (Filename.basename
                                                        Sys.executable_name)

let readFromStdIn=ref false

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
 Printf.eprintf "%s\n%s\n@." (Pos.locToString
                               (Lexing.lexeme_start_p lexbuf,
                                Lexing.lexeme_end_p lexbuf)) s;
 exit 1

let parse fname chan=
 let lb = Lexing.from_channel chan in
 try
  Lexer.setFile lb fname;
  Parser.program Lexer.token lb
 with
  | Lexer.Lexical_error s ->
     parseError lb (Printf.sprintf "lexical error: %s" s)
  | Parsing.Parse_error ->
     parseError lb (Printf.sprintf "syntax error in parse rule: \"%s\""
                     !ParseInfo.currentRule)

let specs=Arg.align (Cps.specs@specs)

let () =
 let file = ref None in
 Arg.parse specs (fun s -> file := Some s) usage;
 try
  let ast=match !file,!readFromStdIn with
   |Some f,false ->
     with_open_in f (parse f)
   | None,true -> parse "<STDIN>" stdin
   | _ -> Arg.usage specs usage; exit 1
  in Cps.compile ast
 with e ->
  Printf.eprintf "Anomaly: %s\n@." (Printexc.to_string e);
  exit 2
