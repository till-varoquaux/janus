(*w
  This file contains many general utility functions. It is yet another extension
  to the standard library.
*)

(*w
  Ocaml's stdlib seems to be strangely lacking of some basic features. This
  module stub is inspired by
  [[http://ocaml-lib.sourceforge.net/doc/Option.html|ExtLib's option
  module]].
*)
module Option=
 struct
  exception No_value

  let get = function
   | Some v -> v
   | None -> raise No_value

  let map f= function
   | Some x -> Some (f x)
   | None -> None

  let map_default f x = function
   | Some v -> f v
   | None -> x

  let default null = function
   | Some v -> v
   | None -> null
 end

module List=
 struct
  include List
  let scan a l =
   let rec aux i = function
    | [] -> raise Not_found
    | x::_ when x=a -> i
    | _::t -> aux (i+1) t
   in
   aux 0 l
 end

module String=
 struct
  include String
   (*w
     Removes ^^n^^ characters from the beginning of a string.
   *)
  let chopStart s n=
   String.sub s n ((String.length s)-n)
 end

module StringMap=Map.Make(String)
module StringSet=Set.Make(String)
module StringHashtbl=Hashtbl.Make (
 struct
  type t=string
  let equal=(=)
  let hash=Hashtbl.hash
 end)

(*w
   Reads the whole content from a channel
*)
let channelToString ic=
 let b=Buffer.create 17 in
 try
  while true do
   Buffer.add_char b (input_char ic)
  done;
  assert false
 with End_of_file ->
  Buffer.contents b

(*w
  Reads an input channel to a list of strings, each string corresponds to a
  line.
*)
let channelToStringList ic=
 let b=Buffer.create 17
 and res=ref [] in
 try
  while true do
   match input_char ic with
    | '\n' ->
       res:= (Buffer.contents b)::!res;
       Buffer.reset b
    | c -> Buffer.add_char b c
  done;
  assert false
 with End_of_file ->
  begin
   List.rev (if Buffer.length b > 0 then
              Buffer.contents b::!res
             else
              !res)
  end
(*w
  ^^atomSeq f g^^
  runs f() then g() and outputs the result of f().
*)
let atomSeq f g=
 let runCloseFun=ref true in
 let closeAtExit ()=
  if !runCloseFun then
   g ()
 in
 at_exit closeAtExit;
 let res=(try
           f ()
          with e ->
           g ();
           raise e)
 in
 runCloseFun := false;
 g ();
 res

(*w
  This is the standard lisp way to proceed with file handles... Suprisingly it
  is cleaner than what we usualy do in ocaml.
*)
let with_open_in filename f=
 let ch=open_in filename in
 atomSeq (fun () -> f ch) (fun () -> close_in ch)

let with_open_out filename f=
 let ch=open_out filename in
 atomSeq (fun () -> f ch) (fun () -> flush ch;close_out ch)

(*w
  We are now handling processes in a Lispish way. This function returns a tuple
  composed of the result and the exit status.
  Todo: stop dumping stderr.
*)
let with_open_process_full process f=
 let chs=Unix.open_process_full process [||] in
 let status=ref None in
 let res=(atomSeq(fun () -> f chs) (fun () -> status:= Some (Unix.close_process_full
                                  chs))) in
 res,(Option.get (!status))
