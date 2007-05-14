{
  open Lexing
  open Parser
  open AstStd

  exception Lexical_error of string

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,k) -> Hashtbl.add h s k)
     [ "if", If; "else", Else; "while", While; "and", And;"not", Not; "var",
        Var;"or", Or; "true", True;"false", False; "function",
        Function;"cps",Cps;"return", Return; "macro" , Macro; "cps_macro" ,
        CpsMacro];
      fun s -> let sl = String.lowercase s in
	try Hashtbl.find h sl with Not_found -> Ident sl

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  type state=
   | Token
   | Macro

  let state=ref Token
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = alpha (alpha | '_' | digit)*
let exponent = ('e' | 'E') ('+' | '-')? digit+
let float = digit+ '.' digit+ exponent?
  | digit+ exponent

rule token = parse
 | '\n'
   { newline lexbuf; token lexbuf }
 | [' ' '\t' '\r']+
   { token lexbuf }
 | "/*"   { comment lexbuf; token lexbuf }
 | "//"
   { comment2 lexbuf; token lexbuf }
 | ident
   { id_or_keyword (lexeme lexbuf) }
 | '_'
   {T}
 | digit+
   { Integer (int_of_string (lexeme lexbuf)) }
 | float
   { Real (float_of_string (lexeme lexbuf)) }
 | '('
   { Lpar }
 | ')'
   { Rpar }
 | '{'
   { Lbracket }
 | '}'
   { Rbracket }
 | '['
   { Lsquare }
 | ']'
   { Rsquare }
 | "->"
   {Arrow}
 | "=>"
   {DOUBLEArrow}
 | ','
   { Comma }
 | ';'
   { Semicolon }
 | ':'
   { Colon }
 | "-"
   { Minus }
 | "+"
   { Plus }
 | "*"
   { Star }
 | "/"
   { Slash }
 | '='
   { Eq }
 | ":="
   { Aff }
 | ">"
   { Comp `Gt }
 | ">="
   { Comp `Ge }
 | "<"
   { Comp `Lt }
 | "<="
   { Comp `Le }
 | "<>"
   { Comp `Neq }
 | "${"
    {state:=Macro;macrobloc lexbuf}
 | _
   { raise (Lexical_error ("illegal character: " ^ lexeme lexbuf)) }
 | eof
   { EOF }

and macrobloc= parse
 | "}$" {state:=Token;token lexbuf}
 | "$" { macroident lexbuf}
 | _  { MacroLITERAL (lexeme lexbuf) }
 | eof  { raise (Lexical_error "unterminated macro") }

and macroident = parse
 | ident {MacroIdent (lexeme lexbuf)}

and comment = parse
  | "*/" { () }
  | '\n' { newline lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Lexical_error "unterminated comment") }

and comment2 = parse
  | "\n"  { () }
  | _    { comment2 lexbuf }
  | eof  { raise (Lexical_error "unterminated comment") }

{
 let lex l=
  match !state with
   | Macro -> macrobloc l
   | Token -> token l
}
