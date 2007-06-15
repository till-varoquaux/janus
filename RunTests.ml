(*w
   This scripts runs all the tests
*)
#use "topfind"
#require "unix"
#require "str"
#use "General.ml"

exception Fail

let usage = "ocaml RunTests.ml compiler"
let spec=[]
let executable =
 let file=ref None in
 Arg.parse spec (fun s -> file := Some s) usage;
 match !file with
  | Some f -> f
  | None -> Arg.usage spec usage; exit 1

let testDir="test"

let tty=((Unix.getenv "TERM")<>"dumb")

let clear () =
 flush stdout;
 if tty then
  Printf.printf "\r\027[K"
 else
  ()

let success s=
 clear();
 Printf.printf "%s" (Filename.chop_extension s);
 if not tty then
  Printf.printf ":success\n"

let faillure ?(failled=[]) s=
 clear ();
 Printf.eprintf "%s: faillure [%s]\n"
  (Filename.chop_extension s)
  (String.concat "," failled);
 flush stderr

let base s=
 Filename.concat testDir (Filename.chop_extension s)

let isTest s=
 (Filename.check_suffix s ".jis") &&
  (Sys.file_exists ((base s)^ ".expected"))

let readProcess p=
 with_open_process_full p (fun (ic,_,_) -> channelToString ic)

let readFile f=
 with_open_in f channelToString

let passes=
 let re=Str.regexp "\n"
 and re2=Str.regexp ":"
 and shopt,_=readProcess (executable^" -shoptpasses")
 in
 let res=List.map
  begin
   fun s -> match (Str.split re2 s) with
    | [x;y] -> y,x
    | _ -> assert false
  end (Str.split re shopt) in
 res

let run ?(opt="") s =
 let script = Filename.concat testDir s in
 let process = Printf.sprintf "%s %s %s | smjs" executable opt script in
 let content,stat=
  try
   readProcess process
  with _ -> raise Fail
 in
 (match stat with
   | Unix.WEXITED 0 -> ()
   | Unix.WEXITED _ -> raise Fail
   | _ -> assert false);
 content

let check ?(opt="") s =
 try
  let expected = readFile ((base s)^ ".expected")
  and result = run ~opt:opt s in
  if result=expected then
   true
  else
   false
 with Fail ->
  false

let test s =
 if check s then
  success s
 else
  begin
   let successfull=List.filter (fun (_,x) -> check ~opt:x s) passes
   in
   faillure ~failled:(List.map fst successfull) s
  end

let tests=
 let fileArray=Sys.readdir testDir in
 let files=Array.to_list fileArray in
 List.filter isTest files


let () =
 List.iter test tests;
 clear ();
 Printf.printf "Done\n"
