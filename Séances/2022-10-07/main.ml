(**
* iLISP (inside LISP)
*   Interpreter entry point
*)

open Asyntax
open Environment
open Lisp

(* initialise the builtin environment *)
let add_builtins () =
  List.iter (fun (name,fn) -> add_function_binding name fn) Builtins.all_builtins


(* the main loop: read-eval-print *)
let _ =
  begin
    (* initialise builtins *)
    add_builtins ();

    (* initialise lexer *)
    let lexbuf = Lexing.from_channel stdin in

    (* call parser *)
    let read () = Parser.parse Lexer.token lexbuf in
    while true do
      try
        let sexp = read () in
        print_endline (string_of_sexp sexp);
        let value = eval sexp in
        print_endline ("=> " ^ (string_of_sexp value));
        flush stdout
      with
        Error s -> print_endline ("=> " ^ s); flush stdout
      | Parsing.Parse_error -> prerr_endline "Parse error";
      | Lexer.Eof -> exit 0
    done
  end









































































