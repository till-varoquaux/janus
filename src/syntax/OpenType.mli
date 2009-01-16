(*w ==Generate the open type==
 * [@OpenType@]
 *
 * Our grammars are extensible this is done by using extensible type definitions
 * this is by defining them as an "open loop".
 *
 * Grammar types are defined under the hood using open reccursion and
 * constraints. They will later on be closed via the type parameter (which is an
 * object type).
 *
 *)
open Camlp4.PreCast

(*w
 * [[#OpenType.gen|gen]] will translate grammars from:
 * %%
 * tree:=`Branch tree,tree | `Leaf elem
 * elem:=int
 * %%
 * to:
 * %%
 * type 'cst tree= [`Branch of 'tree,'tree | `Leaf of 'elem]
 * constraint 'cst=<tree:'tree;elem:'elem;..>
 * type 'cst elem=
 * constraint 'cst=<tree:'tree;elem:'elem;..>
 * %%
 * The parameter ^^'cst^^ will later on be used to close the loop.
 *)

val gen: Loc.t -> Grammar.t -> Ast.str_item

(*w
 * [[#OpenType.gen|close]] can then be used to close the open reccursion an get
 * rid of the type parameter. Suppose the previous grammar was in a module
 * called ^^Gram^^ we would obtain the corresponding closed types:
 * %%
 * type cst=<tree:tree;elem:elem>
 * and tree=cst Gram.tree
 * and elem=cst Gram.elem
 * %%
 *)
val close: Loc.t -> Grammar.t -> Ast.ident -> Ast.str_item

(*w
 *
 * Generate the object type used to close a open type. That is, assuming we
 * have a grammar containing three rules: ^^a^^,^^b^^ and ^^c^^ this will
 * generate the following type:
 * %%
 * <a:a;b:b;c:c>
 * %%
 *)
val loopbackType: Loc.t -> Grammar.t -> Ast.ident -> Ast.ctyp
