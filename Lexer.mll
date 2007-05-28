{
  open Lexing
  open Parser
  open AstStd

  exception Lexical_error of string

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,k) -> Hashtbl.add h s k)
     [ "if", If; "else", Else; "while", While; "and", And;"not", Not; "var",
        Var;"or", Or; "function", Function;"cps",Cps;"return", Return; "macro" ,
        Macro; "cps_macro" , CpsMacro];
    fun s -> try Hashtbl.find h s with Not_found -> Ident s

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
   { Constant (`Int (int_of_string (lexeme lexbuf))) }
 | float
   { Constant (`Float (float_of_string (lexeme lexbuf))) }
 | '.'
   { Dot }
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
   {DoubleArrow}
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
 | "true"
   { Constant (`Bool true)}
 | "false"
   { Constant (`Bool false)}
 | "\""
   { let b=Buffer.create 17 in
     string (Buffer.add_string b) lexbuf;
     let s=Buffer.contents b in
     Constant (`String s) }
 | "${"
    {state:=Macro;macrobloc lexbuf}
 | _
   { raise (Lexical_error ("illegal character: " ^ lexeme lexbuf)) }
 | eof
   { EOF }

and escape a = parse
 | "\\" | "\"" {a (lexeme lexbuf)}
 | "\t" {a "\t"}
 | "\n" {a "\n"}
 | _ { raise
        (Lexical_error
          ("illegal escape character in string: " ^ lexeme lexbuf)
        )}

and string a = parse
 | "\"" { () }
 | "\\" { escape a lexbuf; string a lexbuf }
 | _ { a (lexeme lexbuf); string a lexbuf }

and macrobloc= parse
 | "}$" {state:=Token;token lexbuf}
 | "$" { macroident lexbuf}
 | _  { MacroLiteral (lexeme lexbuf) }
 | eof  { raise (Lexical_error "unterminated macro") }

and macroident = parse
 | ident {Ident (lexeme lexbuf)}

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
