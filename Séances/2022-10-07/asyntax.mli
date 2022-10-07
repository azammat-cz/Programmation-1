(**
* iLISP (inside LISP)
*   Abstract syntax tree (AST) definition and utilities
*)

(* raised for incorrect syntax *)
exception Error of string

(* type of AST *)
type sexp = 
    Atom of string
  | Cons of sexp * sexp


(* printing AST into a string *)
val string_of_sexp : sexp -> string

(* test if atom node *)
val atomp : sexp -> bool
(* test if Cons node *)
val listp : sexp -> bool
(* test if nil atom *)
val atomlistp : sexp -> bool
(* test if nil tree *)
val is_nil : sexp -> bool

(* getter for atom's name *)
val unbox_atom : sexp -> string
(* getter for lists: transform sexp into a list *)
val unbox_list : sexp -> sexp list 
(* getter for atom list: transform sexp into a list of atoms *)
val unbox_atomlist : sexp -> sexp list

