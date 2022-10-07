(**
* iLISP (inside LISP)
*   Builtin functions
*)

open Environment

val all_builtins : (string * fn_binding) list

(* Special forms: arguments are not yet evaluated *)
val eval_setq : builtin_fn2
val eval_defun : builtin_fnN
val eval_let : builtin_fnN
val eval_letstar : builtin_fnN
val eval_quote : builtin_fn1
val eval_if : builtin_fnN
val eval_cond : builtin_fn1

(* Ordinary functions: arguments are already evaluated *)
val eval_car : builtin_fn1
val eval_cdr : builtin_fn1
val eval_cons : builtin_fn2
val eval_equal : builtin_fn2
val eval_progn : builtin_fnN
