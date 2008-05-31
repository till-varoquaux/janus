(*w
 * ====Main====
 * This is the entry point of the programm it reads command line options and
 * contains the chain of all of the compiler passes.
 *)
open General

let usage = Printf.sprintf "Usage: %s [options] file...
Options:" (Filename.basename Sys.executable_name)

let readFromStdIn=ref false

let version ()=
 Printf.printf "The %s mini_js compiler, version: %s\n" Version.name
  Version.version;
 Printf.printf "Configured with:\n\t%s\n" Version.configure;
 exit 0

let specs=["-v",Arg.Unit version,"Print the version and exit";
           "-tex-out",Arg.Unit Printer.setTexFormat,"Pretty prints the trees " ^
            "as tex code";
           "-html-out",Arg.Unit Printer.setHtmlFormat,"Pretty prints the trees"
            ^ " as html code";
            "-stdin",Arg.Set readFromStdIn,"Read data from standard in"
          ]

let specs=Arg.align (Cps.specs@specs)

let () =
 let file = ref None in
 Arg.parse specs (fun s -> file := Some s) usage;
 (*try*)
 let ast=match !file,!readFromStdIn with
  |Some f,false ->
    with_open_in f (Cps.parse f)
  | None,true -> Cps.parse "<STDIN>" stdin
  | _ -> Arg.usage specs usage; exit 1
 in print_string (Cps.compile ast)
 (*with e ->
  Printf.eprintf "Anomaly: %s\n@." (Printexc.to_string e);
  exit 2*)
