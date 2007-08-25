open Pos
type p=string pos
gram extends AstCps {
 import ty,ident,expr,instr,constant,lvalue,unop,binop,program;
 ident := p;
 expr := `Pos location,expr | super;
 instr := `Pos location,instr | super
}

let rec lval2expr : lvalue->expr = function
 | `Ident i -> `Ident i
 | `ObjAccess (l,i) -> `ObjAccess (lval2expr l,i)
 | `ArrayAccess (l,e) -> `ArrayAccess (lval2expr l,e)
