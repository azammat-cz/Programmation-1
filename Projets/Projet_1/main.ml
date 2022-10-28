(*
Fichier main.ml
Fichier principal du projet.
*)

open Asyntax
open Lexer
open Parser
open Compiler
open X86_64

let main file =
  let f = open_in file in
  try
    let lexbuf = Lexing.from_channel f in
    let ast = Parser.main Lexer.token lexbuf in
    let tp = check_ast ast in
      if tp = I then (print_in_file "expression.s" (compile ast I); print_string "OK, fichier expression.s généré.")
      else if tp = F then (print_in_file "expression.s" (compile ast F); print_string "OK, fichier expression.s généré.")
  with
    | Asyntax.BadType -> print_string "Mauvais typage d'AST, vérifiez que vous ne mêlez pas entiers et flottants dans vos opérations."; exit 1
    | _ -> print_string "Erreur de syntaxe, vérifiez que vous écrivez bien une expression arithmétique en notation infixe."; exit 1;;

main Sys.argv.(1);;
