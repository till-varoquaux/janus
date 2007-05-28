(*w
  This file contains many general utility functions. It is yet another extension
  to to standard library.
*)
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
