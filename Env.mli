(*w
   This module codes cps typing environements. Typing environement bind some
   variables to a given type, it can also hold macros and tell us wether we are
   in an cps block or not.
*)
open AstStd
open General
type id=AstCpsInt.ident
type t
type ty'=AstCpsInt.ty
val add:ident->ty'->t->t
val ty:ident->t->ty'
val ident:ident->t->id
val empty:t
val oldify:t->t

(**
   Generates a fresh identifier.
 *)
val fresh:?hint:string->unit->id

(**
   Are we in a cps block
*)
val cps:t->bool

 (**
    Changes the cps value of an environement. This tells us wether we are in a
    cps block or not.
 *)
val setCps:bool->t->t
