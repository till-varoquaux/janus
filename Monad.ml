(*w
  This module contains a very straightforward definition of monads. Monads are
  used in our tree traversal functions to ensure flexibility. Allthough they are
  a great tool they stil don't solve all our problems for instance memoization
  can't be easilly added via monads...

  [[http://www.haskell.org/arrows/|Arrows]] could prove a viable alternative.
*)
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

(*w
  Identity monad.
*)
module Id=
struct
 type 'a m='a
 let return a=a
 let bind a f= f a
 let run a=a
end;;

(*w
   State monad.
*)
module type State=
 sig
  type t
 end;;


module StateMonad(M:State)=
struct
 type 'a m= M.t -> ('a*M.t)
 let run f v = fst (f v)
 let bind (f:'a m) (g:'a -> 'b m) : 'b m=
  (fun x ->( let (a,x)= f x in
            (g a) x
          ));;
 let return a=fun d -> (a,d)
end;;
