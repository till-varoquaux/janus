open General
open Pos
open AstCpsInt
module C=Map.Make(String)
type id=AstCpsInt.ident
type ty'=AstCpsInt.ty

(*w
  This is a list of reserved keywords. Since they are used in javascript we do
  not want to override them.
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
  (**Objects*)
  "Array";"Boolean";"Date";
  "Error";"Function";"java";
  "JavaArray";"JavaClass";"JavaObject";
  "JavaPackage";"Math";"netscape";
  "Number";"Object";"Packages";
  "RegExp";"String";"sun";
   (**Properties*)
  "Infinity";"NaN";"undefined";
   (**Functions*)
  "decodeURI";"decodeURIComponent";"encodeURI";
  "encodeURIComponent";"eval";"isFinite";
  "isNaN";"parseFloat";"parseInt"
]


let fresh=
 let module M=StringHt
 in
 let dec=M.create 89 in
 List.iter (fun i -> M.add dec i 0) reservedKeywords;
 fun ?(hint="fresh")() ->
  if M.mem dec hint then begin
   let cnt=M.find dec hint in
   M.remove dec hint;
   M.add dec hint (cnt+1);
   Printf.sprintf "%s$%i" hint cnt
  end else begin
   M.add dec hint 0;
   Printf.sprintf "%s" hint
  end;;

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
  {env with recent=C.add i (name,(ty:>ty)) env.recent}

let ident {node=i;loc=_} env =
 try
  fst (C.find i env.recent)
 with Not_found ->
  (try fst (C.find i env.old)
   with Not_found -> i
  )

let ty {node=i;loc=_} env=
 try
  snd (C.find i env.recent)
 with Not_found ->
  (try snd (C.find i env.old)
   with Not_found -> `T
  )

let empty=
 {old=C.empty;
  recent=C.empty;
  cps=true}

let cps e=
 e.cps

let setCps b e=
 {e with cps=b}

let oldify t =
 {t with
   old=C.fold (fun k e env -> C.add k e env) t.recent t.old;
   recent=C.empty}
