open Pos
open AstCpsInt
module C=Map.Make(String)
type id=AstCpsInt.ident
type ty'=AstCpsInt.ty

let fresh=
 let cnt=ref 0 in
 fun ?(hint="fresh")() ->
  cnt:=!cnt+1;
  Printf.sprintf "$%s_%i" hint !cnt

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
