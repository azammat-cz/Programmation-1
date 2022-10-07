(**
* iLISP (inside LISP)
*   Abstract syntax tree (AST) definition and utilities
*)

exception Error of string

type sexp = 
    Atom of string
  | Cons of sexp * sexp

let atomp sexp =
  match sexp with
    Atom _ -> true
  | Cons _ -> false


let rec listp sexp = 
  match sexp with 
    Atom "nil" -> true
  | Atom _ -> false
  | Cons (car,cdr) -> listp cdr


let rec atomlistp sexp =
  match sexp with 
    Atom "nil" -> true
  | Atom _ -> false
  | Cons (car,cdr) ->
      (atomp car) && (atomlistp cdr)


let is_nil sexp =
  match sexp with
    Atom s -> (s = "nil")
  | Cons _ -> false
      

let unbox_atom sexp =
  match sexp with
    Cons _ -> raise (Stdlib.Invalid_argument "Trying to unbox an list as atom")
  | Atom s -> s


let rec unbox_atomlist sexp =
  match sexp with
    Atom "nil" -> []
  | Atom _ -> raise (Error "This isn't a list of atoms")
  | Cons (car,cdr) ->
      match car with
	Atom s -> (Atom s)::(unbox_atomlist cdr) 
      |	Cons _ -> raise (Error "This isn't a list of atoms") 


let rec unbox_list sexp =
  match sexp with
    Atom "nil" -> []
  | Atom _ -> raise (Error "This isn't a list")
  | Cons (car,cdr) -> car::(unbox_list cdr)


let rec string_of_sexp sexp =
  match sexp with
    Atom "nil" -> "nil"
  | Atom name -> name
  | Cons (Atom "quote", Cons (rest,Atom "nil")) ->  "'" ^ (string_of_sexp rest)
  | Cons (car,cdr) ->
      if (listp sexp)
      then 
	let l = unbox_list sexp in
	"(" ^ (String.concat " " (List.map string_of_sexp l)) ^ ")"
      else 
	"(" ^ (string_of_sexp car) ^
	" . " ^ (string_of_sexp cdr) ^ ")"

