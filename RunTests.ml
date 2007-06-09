(*w
   This scripts runs all the tests
*)
#use "topfind"
#require "unix"
#require "str"
#use "General.ml"

exception Fail

let executable="./Main.byte"
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
 Printf.eprintf "%s: faillure [%s]\n"
  (Filename.chop_extension s)
  (String.concat "," failled)

let base s=
 Filename.concat testDir (Filename.chop_extension s)

let isTest s=
 (Filename.check_suffix s ".jis") &&
  (Sys.file_exists ((base s)^ ".expected"))

(*w
   Reads the whole content from a channel
*)
let readChannel ic=
 let b=Buffer.create 17 in
 try
  while true do
   Buffer.add_char b (input_char ic)
  done;
  assert false
 with End_of_file ->
  Buffer.contents b

let readProcess p=
 with_open_process_full p (fun (ic,_,_) -> readChannel ic)

let readFile f=
 with_open_in f readChannel

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
