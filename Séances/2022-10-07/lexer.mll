(**
* iLISP (inside LISP) 
*   Lexical analyser written in ocamllex
*)
{
 open Parser
 exception Eof
} 

(** token's grammar
*    ocamllex generates token function, 
*    called in module Main
*)
rule token = parse
  [' ' '\t' '\n'] { token lexbuf } (* skip spaces and empty lines *)
| '('      { LBRACE }
| ')'      { RBRACE }
| '\''     { QUOTE }    
| '.'      { DOT }
| ';' [^ '\n']*  { token lexbuf }  (* skip comments starting with ; until newline *)
| ['A'-'z' '0'-'9' '*']+ { NAME(Lexing.lexeme lexbuf) } (* return name *)
| eof { raise Eof } (* signal end of file *)
    

