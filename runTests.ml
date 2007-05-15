(**
   This scripts runs all the tests
*)
#use "topfind"
#require "unix"

let executable="_build/Main.byte"
let testDir="test"

let guard_process chan f g=
 let res=(try
           f chan
          with e ->
           g chan;
           raise e)
 in
 g chan;
 res

let with_open_in filename f=
 guard_process (open_in filename) f close_in

(**
   Todo: stop dumping stderr
*)
let with_open_process filename f=
 let inc,outc,errc=Unix.open_process_full filename [||] in
 let res=(try
           f inc
          with e ->
           ignore(Unix.close_process_full (inc,outc,errc));
           raise e)
 in
 res,Unix.close_process_full (inc,outc,errc)

let success s=
 Printf.printf "%s: success\n" (Filename.chop_extension s)

let faillure s=
 Printf.eprintf "%s: faillure\n" (Filename.chop_extension s);
 exit 1

let base s=
 Filename.concat testDir (Filename.chop_extension s)

let isTest s=
 (Filename.check_suffix s ".jis") &&
  (Sys.file_exists ((base s)^ ".expected"))

(**
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

let readFile f=
 with_open_in f readChannel

let run s =
 let script = Filename.concat testDir s in
 let process = Printf.sprintf "%s %s | smjs" executable script in
 let content,stat=
  try
   with_open_process process readChannel
  with _ -> faillure s
 in
 (match stat with
   | Unix.WEXITED 0 -> ()
   | Unix.WEXITED _ -> faillure s
   | _ -> assert false);
 content

let check s =
 let expected = readFile ((base s)^ ".expected")
 and result = run s in
 if result=expected then
  success s
 else
  faillure s

let tests=
 let fileArray=Sys.readdir testDir in
 let files=Array.to_list fileArray in
 List.filter isTest files

let () = List.iter check tests
