/**
* iLISP (inside LISP)
*   Syntax analyser written in ocamlyacc
*/

/* declaration of tokens from Lexer */
%token <string> NAME 
%token LBRACE RBRACE EOF QUOTE DOT

/* start symbol of the grammar */
%start parse

/* type of syntax tree built by the start symbol */
%type <Asyntax.sexp> parse

%%

parse:  
  sexp   { $1 }
;

sexp:
  list { $1 }
| atom { $1 }
| QUOTE sexp { Asyntax.Cons (Asyntax.Atom "quote", Asyntax.Cons($2, Asyntax.Atom "nil")) }
;

list:  
  LBRACE RBRACE { Asyntax.Atom "nil" }
| LBRACE inside_list RBRACE { $2 }
;

inside_list:
  sexp DOT sexp { Asyntax.Cons ($1,$3) }
| sexp { Asyntax.Cons ($1, Asyntax.Atom "nil") }
| sexp inside_list {Asyntax.Cons($1,$2)}
;

atom: NAME { Asyntax.Atom $1 }
;



