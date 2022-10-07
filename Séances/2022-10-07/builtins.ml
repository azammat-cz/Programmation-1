(**
* iLISP (inside LISP)
*   Builtin functions
*)

open Asyntax
open Environment
open Lisp

let rec length args =
  match args with
    Atom "nil" -> 0
  | Atom _ -> raise (Error "Not a sequence")
  | Cons (car,cdr) -> 1 + length cdr


let check_min_nargs args min name =
  if length args < min
  then raise (Error ("Wrong number of arguments: " ^ name ^ "," ^ string_of_int min))


let rec last sexp =
  match sexp with
    Atom "nil" -> Atom "nil"
  | Atom _ -> raise (Error "last can't be applied to lists")
  | Cons (car,cdr) ->
    if cdr = Atom "nil"
    then car
    else last cdr


let rec nth n sexp =
  match sexp with
    Atom _ -> raise (Error "nth can only be applied to lists")
  | Cons (car,cdr) ->
    if n = 0 then car
    else nth (n-1) cdr


let rec drop n sexp =
  match sexp with
    Atom "nil" -> Atom "nil"
  | Atom _ -> raise (Error "drop can only be applied to lists")
  | Cons (car,cdr) ->
    if n = 0 then sexp
    else drop (n-1) cdr


(* interpret (quote arg) *)
let eval_quote arg = arg


(* interpret (car arg) *)
let eval_car arg =
  match arg with
    Atom "nil" -> Atom "nil"
  | Atom _ -> raise (Error "car only works on lists")
  | Cons (car,cdr) -> car


(* interpret (cdr arg) *)
let eval_cdr arg =
  match arg with
    Atom "nil" -> Atom "nil"
  | Atom _ -> raise (Error "cdr only works on lists")
  | Cons (car,cdr) -> cdr


(* interpret (cons elt list) *)
let eval_cons elt list =
  Cons (elt, list)

(************************************************************)


(* interpret (add (list of int)) *)
let eval_add sexp =
        let rec aux s = match s with
        | Atom "nil" -> raise (Error "add needs at least 1 argument")
        | Atom(x) -> int_of_string x
        | Cons(f,Atom "nil") -> aux f
        | Cons(f,g) -> (aux f)+aux g
        in Atom(string_of_int (aux sexp));;
        
(* interpret (sub arg) and (sub arg1 arg2) *)
let eval_sub arg1 arg2 = match arg1,arg2 with
        | Atom(x),Atom(y) -> Atom(string_of_int (int_of_string(x) - int_of_string(y)))
        | _ -> raise (Error "sub takes 2 arguments");;

(* interpret (mul (list of int)) *)     
let eval_mul sexp =
        let rec aux s = match s with
        | Atom "nil" -> raise (Error "mul needs at least 1 argument")
        | Atom(x) -> int_of_string x
        | Cons(f,Atom "nil") -> aux f
        | Cons(f,g) -> (aux f)*aux g
        in Atom(string_of_int (aux sexp));;

(* interpret (leq arg1 arg2) *)
let eval_leq arg1 arg2 = match arg1,arg2 with
        | Atom(x),Atom(y) -> if (int_of_string x)<=(int_of_string y) then Atom "t" else Atom "nil";
        | _ -> raise (Error "<= takes 2 arguments");;

(************************************************************)


(* interpret (setq atom value) *)
let eval_setq atom value =
  if not (atomp atom)
  then raise (Error "set can only be used to bind atoms")
  else
    let e_value = eval value in
    (Environment.add_variable_binding (unbox_atom atom) e_value;
     e_value)


(* interpret (defun args) *)
let eval_defun args =
  begin
    prerr_endline ("Evaluating defun with " ^ string_of_sexp args);
    check_min_nargs args 3 "defun";
    let name = nth 0 args in
    let arglist = nth 1 args in
    let body = drop 2 args in
    begin
      prerr_endline ("Defn is " ^ string_of_sexp body);

      if not (atomp name)
      then raise (Error "defun takes atom as first arg");

      if not (atomlistp arglist)
      then raise (Error "defun takes argument list as second argument");

      let lambda = Cons (Atom "lambda", Cons(arglist,body)) in
      (Environment.add_function_binding (unbox_atom name) (Binding lambda);
       name)
    end
  end


(* get last value of sexp args *)
let eval_progn args =
  if is_nil args
  then Atom "nil"
  else last args


(* interpret (if cond then_form else_forms) *)
let eval_if args =
  begin
    check_min_nargs args 2 "if";
    let cond_form = nth 0 args in
    let then_form = nth 1 args in
    let else_forms = drop 2 args in
    let e_cond_form = eval cond_form in
    if not (is_nil e_cond_form)
    then
      eval then_form
    else
      let e_else_forms = eval_list else_forms in
      eval_progn e_else_forms
  end


(* interpret cond: Returns Some x if clause succeded, else None *)
let rec eval_clause sexp =
  match sexp with
    Atom "nil" -> None
  | Atom _ -> raise (Error "cond clause can't be an atom")
  | Cons (car,cdr) ->
    match eval car with
      Atom "nil" -> None
    | success_value ->
      (Some
	 (if is_nil cdr then success_value
          else eval_progn (eval_list cdr)))

(* interpret (cond clause1 ... clauseN) *)
let rec eval_cond arg =
  match arg with
    Atom "nil" -> Atom "nil"
  | Atom _ -> raise (Error "cond takes a list")
  | Cons (car,cdr) ->
    let result = eval_clause car in
    match result with
      Some sexp -> sexp
    | None -> eval_cond cdr


(* interpret (equal arg1 arg2) *)
let eval_equal arg1 arg2 =
  let rec equal a b =
    match a with
      Atom s1 ->
      begin match b with
	  Atom s2 -> s1 = s2
	| Cons _ -> false
      end
    | Cons (car1,cdr1) ->
      begin match b with
	  Atom _ -> false
	| Cons (car2,cdr2) -> equal car1 car2 && equal cdr1 cdr2
      end
  in

  if equal arg1 arg2
  then Atom "t"
  else Atom "nil"


(* interpret one binding in let ((name1 sexpr1) ...) ...) *)
let eval_binding binding =
  match binding with
    Atom "nil" -> Atom "nil"
  | Atom _ -> raise (Error "binding can't be an atom")
  | Cons(sym,Cons(rest,Atom "nil")) ->
    let e_rest = eval rest in
    Cons(sym,Cons(e_rest,Atom "nil"))
  | Cons _ -> raise (Error "Binding can only have one value")


(* interpret list of bindings in let ((name1 sexpr1) ...) ...) *)
let rec map_eval_bindings bindings =
  match bindings with
    Atom "nil" -> Atom "nil"
  | Atom _ -> raise (Error "list of bindings can't be an atom") 
  | Cons (car,cdr) ->
    let car' = eval_binding car in
    let cdr' = map_eval_bindings cdr in
    Cons (car',cdr')


(* interpret (let (bindings) body *)
let eval_let arg =
  begin
    check_min_nargs arg 1 "let"; (* body is optional *)
    let bindings = nth 0 arg in
    let body = drop 1 arg in
    let e_bindings = map_eval_bindings bindings in
    begin
      iter_bindings e_bindings JustAdd; (* side effect on environment *)
      let e_body = eval_list body in
      let result = eval_progn e_body in
      (iter_bindings bindings Remove; (* side effect of environement *)
       result)
    end
  end

(* interpret (let* (seq_bindings) body ) *)
let eval_letstar arg =
  begin
    check_min_nargs arg 1 "let*";
    let seq_bindings = nth 0 arg in
    let body = drop 1 arg in
    begin
      iter_bindings seq_bindings EvalThenAdd;
      let e_body = eval_list body in
      let result = eval_progn e_body in
      (iter_bindings seq_bindings Remove;
       result)
    end
  end

(* table of builtin functions *)
let all_builtins =
  [ (* Special forms *)
    "setq", SpecialForm2 eval_setq;
    "defun", SpecialFormN eval_defun;
    "let", SpecialFormN eval_let;
    "let*", SpecialFormN eval_letstar;
    "quote", SpecialForm1 eval_quote;
    "cond" , SpecialFormN eval_cond;
    "if", SpecialFormN eval_if;

    (* Vanilla builtins *)
    "car", BuiltinFn1 eval_car;
    "cdr", BuiltinFn1 eval_cdr;
    "cons", BuiltinFn2 eval_cons;
    "equal" , BuiltinFn2 eval_equal;
    "progn", BuiltinFnN eval_progn;
    "add", BuiltinFnN eval_add;
    "sub", BuiltinFn2 eval_sub;
    "mul", BuiltinFnN eval_mul;
    "leq", BuiltinFn2 eval_leq;
  ]

