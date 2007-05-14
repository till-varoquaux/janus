%{
 open General

  let loc() = symbol_start_pos (), symbol_end_pos ()
  let toLoc n = {node = n ; loc = loc() }
  let mk_ident = toLoc
  let mk_expr e = `Pos (loc (),e)
  let mk_instr i = `Pos (loc (),i)
%}


%token <string> Ident
%token <int> Integer
%token <float> Real

/* Mots cles */

%token If Else While And Not Var Mod EOF
%token Function Or True False Return Cps T Macro CpsMacro
%token <string> MacroIdent
%token <string> MacroLITERAL

/* Symboles */

%token Lpar Rpar
%token Lsquare Rsquare
%token Lbracket Rbracket
%token Semicolon Comma
%token Colon
%token Arrow DOUBLEArrow

/* Operators  */

%token Aff // :=
%token Eq // =
%token <Ast_std.binop> Comp
%token Plus Minus
%token Star Slash

/* Precedence  */
%nonassoc If
%nonassoc Else

%left Comp Eq
%left Plus Minus Or
%left Star Slash Mod And
%nonassoc uminus
%nonassoc unot

  /* Entry point  */

%start program
%type <Ast_std.program> program

%%

tyd:
|a_tyd  { ParseInfo.set_current_rule "tyd"; $1 }
a_tyd:
| Colon ty {$2}
| {`T}

arg:
|a_arg  { ParseInfo.set_current_rule "arg"; $1 }
a_arg:
| ident tyd        {($1,$2)}

args:
|a_args  { ParseInfo.set_current_rule "args"; $1 }
a_args:
| /* epsilon */          { [] }
| arg args2              { $1::$2}

args2:
|a_args2  { ParseInfo.set_current_rule "args2"; $1 }
a_args2:
| /* epsilon */          { []     }
| Comma arg args2        { $2::$3 }

constant:
|a_constant  { ParseInfo.set_current_rule "constant"; $1 }
a_constant:
| True   { `Bool(true)  }
| False  { `Bool(false) }
| Integer { `Int($1)     }
| Real   { `Float($1)   }

expr1:
|a_expr1  { ParseInfo.set_current_rule "expr1"; $1 }
a_expr1:
| Lpar expr Rpar          { $2 }
| constant        { `Cst $1 }
| lvalue          { `Lval $1}
| fundecl Lpar args Rpar  tyd bloc {
   let args,args_ty= List.split $3 in
   let f=`Fun (args,$6) in
   let ty=(if $1 then
            `CpsArrow (args_ty,$5)
           else
            `Arrow (args_ty,$5)
          )
   in
   `Typed(f,ty)}
| expr1 Lpar exprs Rpar { `Call ($1,$3)}


expr_aux:
|a_expr_aux  { ParseInfo.set_current_rule "expr_aux"; $1 }
a_expr_aux:
| Not expr   %prec unot   { `Unop (`Not,$2) }
| Minus expr %prec uminus { `Unop (`Minus,$2) }
| expr Eq expr            { `Binop (`Eq,$1, $3)}
| expr Comp expr          { `Binop ($2, $1, $3)}
| expr Plus expr          { `Binop (`Add, $1, $3)}
| expr Minus expr         { `Binop (`Sub, $1, $3)}
| expr Star expr          { `Binop (`Mul, $1, $3)}
| expr Slash expr         { `Binop (`Div, $1, $3)}
| expr Mod expr           { `Binop (`Mod, $1, $3)}
| expr And expr           { `Binop (`And, $1, $3)}
| expr Or expr { `Binop (`Or, $1, $3)}
| expr1 {$1}

fundecl:
|a_fundecl  { ParseInfo.set_current_rule "fundecl"; $1 }
a_fundecl:
| Function {false}
| Cps {true}

expr:
|a_expr  { ParseInfo.set_current_rule "expr"; $1 }
a_expr:
| expr_aux {mk_expr $1}

exprs:
|a_exprs  { ParseInfo.set_current_rule "exprs"; $1 }
a_exprs:
| /* epsilon */          { [] }
| expr exprs2            { $1::$2}

exprs2:
|a_exprs2  { ParseInfo.set_current_rule "exprs2"; $1 }
a_exprs2:
| /* epsilon */          { []     }
| Comma expr exprs2      { $2::$3 }

lvalue:
|a_lvalue  { ParseInfo.set_current_rule "lvalue"; $1 }
a_lvalue:
| ident                        { `Ident($1)    }
| lvalue Lsquare expr  Rsquare { `Array($1,$3) }

cond:
|a_cond  { ParseInfo.set_current_rule "cond"; $1 }
a_cond:
| Lpar expr Rpar {$2}

macroelem:
|a_macroelem  { ParseInfo.set_current_rule "macroelem"; $1 }
a_macroelem:
| MacroIdent {`Ident $1}
| MacroLITERAL {`Literal $1}

macrobloc:
|a_macrobloc  { ParseInfo.set_current_rule "macrobloc"; $1 }
a_macrobloc:
| /**/ {[]}
| macroelem macrobloc {$1::$2}

instr:
|a_instr  { ParseInfo.set_current_rule "instr"; $1 }
a_instr:
| instr_aux {mk_instr $1}

instr_aux:
|a_instr_aux  { ParseInfo.set_current_rule "instr_aux"; $1 }
a_instr_aux:
| Macro ident Lpar args Rpar  macrobloc {
   let args,args_ty= List.split $4 in
   `Macro ($2,args,$6,args_ty)}
| CpsMacro ident Lpar args Rpar  macrobloc {
   let args,args_ty= List.split $4 in
   `CpsMacro ($2,args,$6,args_ty)}
| Var ident Eq expr  {`Vardecl ($2,$4)}
| lvalue Aff expr {`Assign ($1,$3)}
| If cond bloc_or_instr  {`If ($2,$3,[])}
| If cond bloc_or_instr Else bloc_or_instr
    {`If ($2,$3,$5)}
| While cond bloc_or_instr {`While ($2,$3)}
| Return expr {`Ret ($2)}
| expr1 Lpar exprs Rpar  {`Call ($1,$3)}

ty:
|a_ty  { ParseInfo.set_current_rule "ty"; $1 }
a_ty:
| T
  { `T }
| Lpar tys Rpar Arrow ty
  { `Arrow ($2,$5) }
| Lpar tys Rpar DOUBLEArrow ty
  { `CpsArrow ($2,$5) }


tys:
|a_tys  { ParseInfo.set_current_rule "tys"; $1 }
a_tys:
| ty tys2        {$1::$2}
| /* epsilon */        { [] }

tys2:
|a_tys2  { ParseInfo.set_current_rule "tys2"; $1 }
a_tys2:
| Comma ty tys2  { $2::$3 }
| /* epsilon */        { [] }

bloc:
|a_bloc  { ParseInfo.set_current_rule "bloc"; $1 }
a_bloc:
| Lbracket instrs Rbracket        { $2 }

bloc_or_instr:
|a_bloc_or_instr  { ParseInfo.set_current_rule "bloc_or_instr"; $1 }
a_bloc_or_instr:
| bloc                            { $1 }
| instr                           {[$1]}

instrs:
|a_instrs  { ParseInfo.set_current_rule "instrs"; $1 }
a_instrs:
| /*epsilon*/                     { [] }
| instr Semicolon instrs          { $1::$3 }
| instr                           { [$1] }

ident:
|a_ident  { ParseInfo.set_current_rule "ident"; $1 }
a_ident:
| Ident { mk_ident $1 }

program:
|a_program  { ParseInfo.set_current_rule "program"; $1 }
a_program:
| instrs { $1 }
