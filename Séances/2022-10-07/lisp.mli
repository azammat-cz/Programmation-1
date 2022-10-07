(**
* iLISP (inside LISP)
*   Interpreter's core
*)

open Asyntax

type binding_action =
    EvalThenAdd
  | JustAdd
  | Remove

val iter_bindings :
    sexp -> binding_action -> unit

(* main task *)
val eval : sexp -> sexp
(* main task for a list *)
val eval_list : sexp -> sexp
(* main task for function call *)
val handle_lambda : sexp -> sexp -> sexp
