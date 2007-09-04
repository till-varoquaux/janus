include AstStd.ClosedDef
include Compile.Pass(
 struct
  type from=program
  type out=AstCpsHoistInt.program
  let trans=CpsTrans.run
  let print=EmitCpsHoistInt.print
  let name="cpshoist"
 end)(CpsHoistInt)

(*w==Parsing==*)
let parseError lexbuf s=
 let pos=(Lexing.lexeme_start_p lexbuf,Lexing.lexeme_end_p lexbuf)
 in
 Pos.error ~pos:pos s

let parse fname chan=
 let lb = Lexing.from_channel chan in
 try
  Lexer.setFile lb fname;
  Parser.program Lexer.token lb
 with
  | Lexer.Lexical_error s ->
     parseError lb (Printf.sprintf "lexical error: %s" s)
  | Parsing.Parse_error ->
     parseError lb (Printf.sprintf "syntax error in parse rule: \"%s\""
                     !ParseInfo.currentRule)
