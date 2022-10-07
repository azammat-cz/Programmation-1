type token =
  | NAME of (string)
  | LBRACE
  | RBRACE
  | EOF
  | QUOTE
  | DOT

val parse :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Asyntax.sexp
