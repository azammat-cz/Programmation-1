(**
* iLISP (inside LISP)
*   Definitions for interpreter's environment
*)

open Asyntax

(* types for builtin functions *)
(*   - with 1 argument: car, cdr *)
type builtin_fn1 = sexp -> sexp
(*   - with 2 arguments: cons,  *)
type builtin_fn2 = sexp -> sexp -> sexp
(*   - with more than 2 arguments: *)
type builtin_fnN = sexp -> sexp

(* type of functions' binding *)
type fn_binding = 
    BuiltinFn1 of builtin_fn1
  | BuiltinFn2 of builtin_fn2
  | BuiltinFnN of builtin_fnN
  | SpecialForm1 of builtin_fn1
  | SpecialForm2 of builtin_fn2
  | SpecialFormN of builtin_fnN
  | Binding of sexp

(* operations on functions' environment *)
val add_function_binding : string -> fn_binding -> unit
val lookup_function : string -> fn_binding

(* operations on variables' environment *)
val add_variable_binding : string -> sexp -> unit
val lookup_variable : string -> sexp 
val remove_variable_binding : string -> unit





