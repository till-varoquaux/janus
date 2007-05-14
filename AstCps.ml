open General

gram extends AstBase {
 import constant,ident,lvalue,unop,binop,program,macrobloc;
 macroitem:= `Ident string | `Literal string;
 instr := `CpsMacro ident,[ident],macrobloc,[ty]
| `Macro ident,[ident],macrobloc,[ty]
| super;
 expr :=  `Fun [ident],instr| `Typed expr,ty | super
}
