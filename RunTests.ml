(**
   This scripts runs all the tests
*)
#use "topfind"
#require "unix"
#require "str"

exception Fail

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
let with_open_process process f=
 let inc,outc,errc=Unix.open_process_full process [||] in
 let res=(try
           f inc
          with e ->
           ignore(Unix.close_process_full (inc,outc,errc));
           raise e)
 in
 res,Unix.close_process_full (inc,outc,errc)

let success s=
 Printf.printf "%s: success\n" (Filename.chop_extension s)

let faillure ?(failled=[]) s=
 Printf.eprintf "%s: faillure [%s]\n"
  (Filename.chop_extension s)
  (String.concat "," failled)

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

let passes=
 let re=Str.regexp "\n"
 and re2=Str.regexp ":"
 and shopt,_=with_open_process (executable^" -shoptpasses") readChannel
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
   with_open_process process readChannel
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

let () = List.iter test tests
