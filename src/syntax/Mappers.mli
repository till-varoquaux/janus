(*w
 *
 * ==Mappers...==
 *
 * The following set of functions are used to define the traversal function. We
 * are providing only one mean of traversal: a monadic map. We (naïvely) hope
 * this is powerfull enough to capture most of the other transformations one
 * could yearn for.
 *
 * The traversal is done using reccursive modules. To achieve reusability and
 * extensibility we are using late bindings. Just like with object we have two
 * modules directky related to this one:
 *
 * - ^^Super^^ is the module we are inheriting from.
 * - ^^Self^^ is bound to the final module we are building.
 *)
open Camlp4.PreCast
open Syntax

val gen:Grammar.t -> Ast.str_item
