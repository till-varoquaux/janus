open Pos

grammar extends AstCps (ty;constant;lvalue;unop;binop;program)=
begin
 ident := String.t pos;
 expr := `Pos (location,expr) | super;
 instr := `Pos (location,instr) | super
end

let rec lval2expr : lvalue->expr = function
 | `Ident i -> `Ident i
 | `ObjAccess (l,i) -> `ObjAccess (lval2expr l,i)
 | `ArrayAccess (l,e) -> `ArrayAccess (lval2expr l,e)
