(*w
  ====Weak hashtables====

  This is an implementation of weak hashtables based on physicall
  equality. Since these hashtables rely on weak pointers they have to be
  compacted from time to time to delete null pointers. The tables are
  automatically resized and in this process they are also compacted.
*)
open General

let hash=Hashtbl.hash

(*w
  ==Weak pointers==

  We are using weak array containing a single element as weak
  pointers. This in a way, is reminescent of pointer arithmetic. Following this
  logic, our functional take an optionnal index (which is by default set to
  1). This index can be seen as an offset.
*)
type 'a weakPtr = 'a Weak.t

(*w
  Since we don't explicitly have weak pointers we use arrays of size 1.
*)
let weakPtr x=
 let ptr=Weak.create 1 in
 Weak.set ptr 0 (Some x);
 ptr

(*w
  We even have null pointer exceptions, just like in Java...
*)
exception NullPointer;;

let getWeak ?(idx=0) ptr=
 match Weak.get ptr idx with
  | Some k -> k
  | None -> raise NullPointer

let eqWeak ?(idx=0) ptr value=
 match Weak.get ptr idx with
  | Some k -> k==value
  | None -> false

let (=:=) x y= eqWeak x y
let check ?(idx=0) ptr = Weak.check ptr idx

let isNull ?(idx=0) ptr = not (Weak.check ptr idx)

(*w ==Generic functions==
*)
(*w
  This is our hashtable type, we have a couple of arrays (^^keys^^ and
  ^^values^^) in which we have no collisions. Collisions are stored in a
  separate table (^^collisions^^).
*)
type ('a,'b) t =
  {
   mutable size:int;
   mutable keys:'a Weak.t;
   mutable values:'b option array;
   mutable collisions:(('a weakPtr*'b) list) array
  }

(*w
  Creates an empty weak hashtable of initial size ^^n^^.
*)
let create n =
 {
  size=0;
  keys=Weak.create n;
  values=Array.make n None;
  collisions=Array.make n []
 }

let mem t key=
 let bucketNum =(hash key) mod (Array.length t.values) in
 ( eqWeak ~idx:bucketNum t.keys key)
 || (List.exists  t.collisions.(bucketNum) ~f:(fun (e,_) -> e =:= key))

let find t key=
 let bucketNum =(hash key) mod (Array.length t.values) in
 if eqWeak ~idx:bucketNum t.keys key then begin
  assert (t.values.(bucketNum)<>None);
  Option.get t.values.(bucketNum)
 end else
  snd (List.find t.collisions.(bucketNum) ~f:(fun (k,_) -> k =:= key))

let iter f t =
 for i = 0 to (Array.length t.values)-1 do
  begin
   match Weak.get t.keys i with
    | Some k -> let v=Option.get t.values.(i) in f k v
    | None -> ()
  end;
  List.iter
   t.collisions.(i)
   ~f:(fun (k,v) ->
        match Weak.get k 0 with
         | Some k -> f k v
         | None -> ())
 done

let fold ~f t ~init =
 let acc=ref init in
 for i = 0 to (Array.length t.values)-1 do
  begin
   match Weak.get t.keys i with
    | Some k -> let v=Option.get t.values.(i) in acc:=f k v !acc
    | None -> ()
  end;
  acc:=List.fold_left t.collisions.(i)
   ~init:!acc
   ~f:(fun  a (k,v) ->
        match Weak.get k 0 with
         | Some k -> f k v a
         | None -> a)
 done;
 !acc

let resize t=
 let dim=2*(t.size)+1 in
 let keys=Weak.create dim
 and values=Array.make dim None
 and collisions=Array.make dim [] in
 t.size <- 0;
 iter
  begin fun k v ->
   t.size <- t.size+1;
   let bucketNum =(hash k) mod dim in
   if isNull ~idx:bucketNum keys then begin
    Weak.set keys bucketNum (Some k);
    values.(bucketNum) <- Some v
   end else begin
    collisions.(bucketNum) <- ((weakPtr k),v)::collisions.(bucketNum)
   end
  end t;
 t.keys <- keys;
 t.values <- values;
 t.collisions <- collisions


let remove t key=
 let bucketNum =(hash key) mod (Array.length t.values) in
 if ( eqWeak ~idx:bucketNum t.keys key) then begin
  match t.collisions.(bucketNum) with
   | (k,v)::tl ->
      t.collisions.(bucketNum) <- tl;
      Weak.set t.keys bucketNum (Some (getWeak k));
      t.values.(bucketNum) <- Some v
   | [] ->
      Weak.set t.keys bucketNum None;
      t.values.(bucketNum) <- None
 end else begin
  let rec rm=function
   | (k,_)::t when eqWeak k key -> t
   | h::t -> h::(rm t)
   | [] -> raise Not_found in
  t.collisions.(bucketNum) <- rm t.collisions.(bucketNum)
 end;
 t.size <- t.size-1

let add t key value=
 let dim=Array.length t.values in
 let bucketNum =(hash key) mod dim in
 if isNull ~idx:bucketNum t.keys then begin
  t.size <- t.size+1;
  Weak.set t.keys bucketNum (Some key);
  t.values.(bucketNum) <- Some value
 end else begin
  let rec insert=function
   | (k,_)::t when isNull k -> (weakPtr key,value)::t
   | h::t -> h::(insert t)
   | [] ->  t.size <- t.size+1; [weakPtr key,value] in
  t.collisions.(bucketNum) <- (insert (t.collisions.(bucketNum)))
 end;
 if 2*t.size > dim then
  resize t

(*w
  Remove all the dead pointers from a hashtable.
*)
let compact t =
 let sz=ref 0 in
 for i = 0 to (Array.length t.values)-1 do
  let l=List.filter t.collisions.(i) ~f:(fun (k,_) -> check k && (incr sz;true)) in
  match Weak.get t.keys i with
   | Some _ -> incr sz; t.collisions.(i) <- l
   | None ->
      begin match l with
       | [] ->
          t.values.(i) <- None;
          t.collisions.(i) <- []
       | (k,v)::col ->
          t.values.(i) <- Some v;
          Weak.set t.keys i (Weak.get k 0);
          t.collisions.(i) <- col
      end
 done;
 t.size <- !sz

(*w
  Compacts and resize a hashtable
*)
let compactAndResize t=
 let sz=ref 0 in
 let elts=fold ~f:(fun k v acc -> incr sz;(k,v)::acc) t ~init:[] in
 let size=2* !sz+1 in
 t.keys<-Weak.create size;
 t.values<-Array.make size None;
 t.collisions<-Array.make size [];
 List.iter elts
  ~f:(fun (k,v) ->
       let bucketNum =(hash k) mod size in
       if isNull ~idx:bucketNum t.keys then begin
        Weak.set t.keys bucketNum (Some k);
        t.values.(bucketNum) <- Some v
       end else begin
        t.collisions.(bucketNum) <- ((weakPtr k),v)::t.collisions.(bucketNum)
       end);
 t

(*w ==Memoization==
*)
(*w
  Uses a weak hashtable to memoize the results of a function.
*)
let memoize f=
 let cache = create 89 in
 fun x ->
  try
   find cache x
  with Not_found ->
   begin
   let r= f x in
   add cache x r;
   r
  end
