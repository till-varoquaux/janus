(*w
  ==Grammar definition==

*)
open Camlp4.PreCast
open Syntax
open General
(*w This is our basic type definition for grammars. ^^super^^ designs the parent
grammar when making an extension.
*)

(* TODO:
   _Add records, tuples, unit and options
   _How do we fit in Sets Maps etc...?
   _How do we re-export variant names and record labels?
*)
(* Break in two levels to allow tuples and polvars.*)
module Types = struct
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


(*w
 * This module is just a set of rules to ease the handling of grammars, and make
 * them more abstract.
 *)
type t =
    {
      rules:Types.ruleRHS String.Map.t;
      super:Ast.ident option
    }

let mem key g =
  String.Map.mem key g.rules

let fold g ~init ~f =
  String.Map.fold g.rules ~init:init ~f:f

let init super gramItems =
  let imports = match super with
    | None -> []
    | Some (_name,imports) -> imports
  in
  {
    super = Option.map fst super;
    rules =
      List.fold_left
        (List.map imports ~f:(fun s -> s,Types.Import) @ gramItems)
        ~init:String.Map.empty
        ~f:fun m (k,v) -> String.Map.add m ~key:k ~data:v
  }

let super g = g.super
