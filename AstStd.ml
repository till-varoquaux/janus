open General
type p=string pos
gram extends AstCps {
 import ty,ident,expr,instr,constant,lvalue,unop,binop,program,macrobloc,macroitem;
 ident := p;
 expr := `Pos location,expr | super;
 instr := `Pos location,instr | super
}
