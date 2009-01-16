open Camlp4.PreCast
open Syntax
open General

module Types : sig
  type ruleRHS=
    | Abstract
    | Import
    | Variant of (string*ruleItem list) list
    | PolVar of branch list
    | Alias of ruleItem

  and branch =
      (*w This \\has\\ to be another polymorphic variant type *)
    | Other of string
    | Labeled of string * ruleItem list
    | Super

  and ruleItem =
    | Option of ruleItem
    | Atom of string
    | Tup of ruleItem list
    | Nativ of Ast.ctyp
    | List of ruleItem
end


type t

val mem : string -> t -> bool
val typeNames : t -> string list
val assocs : t -> (string*Types.ruleRHS) list

val init : (Ast.ident * string list) option -> (string * Types.ruleRHS) list -> t
val super : t -> Ast.ident option
