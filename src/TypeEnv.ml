(*w ====Cps typing environement ====
 *
 * Refer to the interface for a description
 *)
open General
open Pos
open AstStd
module C=String.Map
type id=AstCpsInt.ident

(*w
 *
 * This is a list of reserved keywords. Since they are used in javascript we do
 * not want to override them.
 *)
let reservedKeywords=[
 (*Ecma 263 reserved words*)
 "break";"else";"new";"var";
  "case";"finally";"return";"void";
  "catch";"for" ;"switch";"while";
  "continue";"function";"this";"with";
  "default";"if";"throw";
  "delete";"in";"try";
  "do";"instanceof";"typeof";
  "abstract";"enum";"int";"short";
  "boolean";"export";"interface";"static";
  "byte";"extends";"long";"super";
  "char";"final";"native";"synchronized";
  "class";"float";"package";"throws";
  "const";"goto";"private";"transient";
  "debugger";"implements";"protected";"volatile";
  "double";"import";"public";
  (*JS 1.5 keywords*)
  (*Objects*)
  "Array";"Boolean";"Date";
  "Error";"Function";"java";
  "JavaArray";"JavaClass";"JavaObject";
  "JavaPackage";"Math";"netscape";
  "Number";"Object";"Packages";
  "RegExp";"String";"sun";
   (*Properties*)
  "Infinity";"NaN";"undefined";
   (*Functions*)
  "decodeURI";"decodeURIComponent";"encodeURI";
  "encodeURIComponent";"eval";"isFinite";
  "isNaN";"parseFloat";"parseInt"
]

(*w
 * A hashtable of all declared variables and the named they are translated to.
 * We use this table to remove all name conflicts (give a unique name to all
 * variables).
 *)
let declared=
 String.Table.create 89

(*w
 * Clears the table of declared variables
 *)
let clear()=
 String.Table.clear declared;
 List.iter reservedKeywords
  ~f:(fun i -> String.Table.add declared ~key:i ~data:0)

let _ = clear()

(*w
 * Generates a new fresh identifier name. It will be unique within the program.
 *)
let fresh=
 let module M=String.Table in
 fun ?(hint="fresh")() ->
  if M.mem declared hint then begin
   let cnt=M.find declared hint in
   M.remove declared hint;
   M.add declared ~key:hint ~data:(cnt+1);
   Printf.sprintf "%s$%i" hint cnt
  end else begin
   M.add declared ~key:hint ~data:0;
   Printf.sprintf "%s" hint
  end;;

(*w
 * This is the type of our environements
 *
 * Since we are checking for roubl declarations within the same scope we have 2
 * pools an old and a recent. If a variable is added whilst there is one with
 * the same name (before renaming) in the recent pool an error is raised.
 *
 * The pools contain two informations:the unique name that was assigned to a
 * given identifier and it's type.
 *)
type t={old:(string*ty) C.t;
        recent:(string*ty) C.t;
        cps:bool
       }

let redef l i=
 error ~pos:l (Printf.sprintf "cannot redefine \"%s\"" i)

let add {node=i;loc=l} (ty:ty) env=
 if (C.mem i env.recent) then
  redef l i
 else
  let name=fresh ~hint:i ()
  in
  {env with recent=C.add ~key:i ~data:(name,(ty:>ty)) env.recent}

(*w
 * Returns the unique identifier name that a given identifier was assigned.
 *)
let ident {node=i;loc=_} env =
 try
  fst (C.find i env.recent)
 with Not_found ->
  (try fst (C.find i env.old)
   with Not_found -> i
  )

(*w
 * Returns the type associated wit a given identifier. If the identifier is
 * unkwnown we type it as a javascript value (T)
*)
let ty {node=i;loc=_} env:AstStd.ty=
 try
  snd (C.find i env.recent)
 with Not_found ->
  (try snd (C.find i env.old)
   with Not_found -> `T
  )

(*w
 * The empty environement
 *)
let empty=
 {old=C.empty;
  recent=C.empty;
  cps=true}

let cps e=
 e.cps

let setCps b e=
 {e with cps=b}

(*w
 * Empties the "recent" pool in the old pool.
 * This is used when are exiting a naming environement (ie it becomes old).
 *)
let newScope t =
 {t with
    old=C.fold
     ~f:(fun ~key:k ~data:e env -> C.add ~key:k ~data:e env)
     ~init:t.recent
     t.old;
    recent=C.empty}
