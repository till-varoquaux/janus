open Pos
open AstCpsInt
module C=Map.Make(String)
type id=AstCpsInt.ident
type ty'=AstCpsInt.ty

let fresh=
 let module M =Hashtbl.Make (
  struct
   type t=id
   let equal=(=)
   let hash=Hashtbl.hash
  end)
 in
 let dec=M.create 89 in
 fun ?(hint="fresh")() ->
  if M.mem dec hint then begin
  let cnt=M.find dec hint in
   M.remove dec hint;
   M.add dec hint (cnt+1);
   Printf.sprintf "$%s_%i" hint cnt
  end else begin
   M.add dec hint 0;
   Printf.sprintf "$%s" hint
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
