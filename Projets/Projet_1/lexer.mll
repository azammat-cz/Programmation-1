(*
Fichier lexer.mll
Nous définissons les tokens pour les expressions arithmétiques.
*)
{
open Lexing
open Parser
exception Eof
}
rule token = parse
    | [' ' '\t'] { token lexbuf }     (* Passer les espaces *)
    | ['\n' ] { EOL }    (* Retour à la ligne *)
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) } (* Entier *)
    | ['0'-'9']+'.'['0'-'9']+ as lxm { FLOAT(float_of_string lxm) } (* Flottant *)
    | "+." { PLUSF }
    | '+' { PLUS }
    | "-." { MINUSF }
    | '-' { MINUS }
    | "*." { MULTF }
    | '*' { MULT }
    | '/' { DIV }
    | "%" { MOD }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | "int" { TOINT }
    | "float" { TOFLOAT }
    | eof { EOL }
    | _ { failwith ( "Unexpected char:" ^ lexeme lexbuf ) } (* Erreur *)
