module type T=
sig
 type 'a m
 val return: 'a -> 'a m
 val bind: 'a m -> ('a -> 'b m) -> 'b m
end

module Helper (Mon:T)=
struct
 type 'a m = 'a Mon.m
 let return= Mon.return
 let (>>=) = Mon.bind

 let rec mmap (f: 'a -> 'b m) = function
  | [] -> return []
  | h::t -> (f h) >>= (fun h ->(
            ((mmap f t) >>= (fun t ->
              return (h::t)
            ))))
end


(*Identity monad*)
module type Runnable=
sig
 include T
 val run: 'a m -> 'a
end

module Id=
struct
 type 'a m='a
 let return a=a
 let bind a f= f a
 let run a=a
end;;


(*state monad*)
module type State=
 sig
  type t
 end;;

(*module type StateMonad =
 functor (M:State) ->
sig
 include T
 val run:'a m -> M.t -> 'a
 (*val get:*)
end*)

module type StateMonad = functor(M:State) ->
sig
 type 'a m=M.t -> ('a*M.t)
 val run: 'a m -> M.t -> 'a
 val bind: 'a m -> ('a -> 'b m) -> 'b m
 val return: 'a -> 'a m
 val readSm: 'a m -> M.t
 val update: 'a m -> M.t -> 'a m
end

module StateMonad
 =
 functor (M:State) ->
struct
 type 'a m= M.t -> ('a*M.t)
 let run f v = fst (f v)
 let bind (f:'a m) (g:'a -> 'b m) : 'b m=
  (fun x ->( let (a,x)= f x in
            (g a) x
          ));;
 let return a=fun d -> (a,d)
 let readSm m = fun s -> s
 let update m s' = fun s -> m s'
end;;
