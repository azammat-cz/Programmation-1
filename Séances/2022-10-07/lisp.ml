(**
* iLISP (inside LISP)
*   Interpreter's core
*)

open Asyntax
open Environment


(* Useful utility when sorting out variable bindings:
   combine (a b c) (1 2 3) gives the result ((a 1) (b 2) (c 3))  *)
let rec combine list1 list2 =
  match list1 with
    Atom "nil" -> Atom "nil"
  | Atom _ -> raise (Error "combine applied to atoms")
  | Cons (car1,cdr1) ->
    begin
      match list2 with
	Atom _ -> raise (Error "Bad lists")
      | Cons (car2,cdr2) ->
	let rest = combine cdr1 cdr2 in
          Cons(Cons(car1, Cons(car2, Atom "nil")), rest)
    end


type binding_action =
    EvalThenAdd
  | JustAdd
  | Remove


(* [process_binding] takes a single binding, like "(x 42)" and performs
   the given binding_action on it to add/remove it to the variable bindings *)
let rec process_binding binding action =
  match binding with
    Atom "nil" -> ()
  | Atom _ -> raise (Error "binding can't be an atom")
  | Cons(sym, Cons(rest, Atom "nil")) ->
    begin
      prerr_endline ("Processing binding " ^ string_of_sexp binding);
      if not (atomp sym)
      then raise (Error "Trying to bind a non-symbol");

      let name = unbox_atom sym in
      (match action with
	 EvalThenAdd ->
           let e_rest = eval rest in
           Environment.add_variable_binding name e_rest
       | JustAdd ->
	  Environment.add_variable_binding name rest
       | Remove ->
	  Environment.remove_variable_binding name
      )
    end
  | Cons _ -> raise (Error "Binding can only have one value")


(* [iter_bindings] take a list of bindings, like "((x 42) (y 43))" and applies the action *)
and iter_bindings bindings action =
  match bindings with
    Atom "nil" -> ()
  | Atom _ -> raise (Error "list of bindings can't be an atom")
  | Cons (car,cdr) ->
    begin
      process_binding car action;
      iter_bindings cdr action
    end


(* Evaluates a list from left-to-right, returning a list of results *)
and eval_list sexp =
  match sexp with
    Atom "nil" -> Atom "nil"
  | Atom _ -> raise (Error ("Trying to eval_list something which isn't a list: " ^ string_of_sexp sexp))
  | Cons (car,cdr) ->
    let car' = eval car in
    let cdr' = eval_list cdr in
    Cons (car',cdr')


(* Interpret function call to arguments ([lambda] [args]) *)
and handle_lambda lambda args =
  (* extract function definition *)
  let match_lambda sexp =
    match sexp with
      Cons (Atom "lambda", Cons (params,defn)) -> (params,defn)
    | _ -> raise (Error "Invalid function binding") in

  let (lambda_params,lambda_defn) = match_lambda lambda in
  begin
    (* parameters shall be a list *)
    if not (atomlistp lambda_params)
    then raise (Error "Not a valid function");

    (* add binding of parameters to respective arguments *)
    let bindings = combine lambda_params args in
    iter_bindings bindings JustAdd;

    (* evaluate body in the new environement *)
    let form = Cons (Atom "progn", lambda_defn) in
    let result = eval form in

    (* remove parameters from the environment *)
    (iter_bindings bindings Remove;
     result)
  end


(* interpreter code, the eval part with some type checking *)
and eval sexp =
  begin
    prerr_endline ("Evaluating: " ^ string_of_sexp sexp);
    match sexp with
    | Atom s ->
      let value = lookup_variable s in
      (prerr_endline (s ^ " has variable binding of " ^ string_of_sexp value);
       value)
    | Cons (car,cdr) ->
      match car with
      | Cons _ -> raise (Error "Invalid function - list")
      | Atom f ->
	begin (* We're evaluating a fn *)
            if not (listp cdr)
            then raise (Error "invalid function");
	    let binding = lookup_function f in
              match binding with
              | BuiltinFn1 fn ->
	        let e_cdr = eval_list cdr in
	        let l = unbox_list e_cdr in
	          if List.length l != 1
	          then raise (Error ("Wrong number of args: " ^ f ^ ",1"))
	          else fn (List.hd l)

	      | BuiltinFn2 fn ->
	        let e_cdr = eval_list cdr in
	        let l = unbox_list e_cdr in
	          if List.length l != 2
	          then raise (Error ("Wrong number of args: " ^ f ^ ",2"))
	          else fn (List.nth l 0) (List.nth l 1)

              | BuiltinFnN fn ->
	        let e_cdr = eval_list cdr in
	          fn e_cdr

	      | SpecialForm1 fn ->
	        let l = unbox_list cdr in
                  if List.length l != 1
	          then raise (Error ("Wrong number of args: " ^ f ^ ",1"))
	          else fn (List.hd l)

              | SpecialForm2 fn ->
	        let l = unbox_list cdr in
	          if List.length l != 2
	          then raise (Error ("Wrong number of args: " ^ f ^ ",2"))
	          else fn (List.nth l 0) (List.nth l 1)

              | SpecialFormN fn ->
	        fn cdr

	      | Binding lambda ->
	        let e_cdr = eval_list cdr in
	          handle_lambda lambda e_cdr
        end
  end

