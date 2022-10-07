(**
* iLISP (inside LISP)
*   Definitions for interpreter's environment
*)

open Printf
open Asyntax

module Symbol =
  struct
    type t = string
    let compare = Stdlib.compare
  end


module SymbolMap = Map.Make(Symbol)

type builtin_fn1 = (sexp -> sexp)
and builtin_fn2 = (sexp -> sexp ->sexp)
and builtin_fnN = (sexp -> sexp)
and fn_binding = 
    BuiltinFn1 of builtin_fn1
  | BuiltinFn2 of builtin_fn2
  | BuiltinFnN of builtin_fnN
  | SpecialForm1 of builtin_fn1
  | SpecialForm2 of builtin_fn2
  | SpecialFormN of builtin_fnN
  | Binding of sexp


let (++) x f = f x

let initial_variables () =
  SymbolMap.empty 
  ++ SymbolMap.add "nil" [Atom "nil"]
  ++ SymbolMap.add "t" [Atom "t"]


let variables : sexp list SymbolMap.t ref 
    = ref (initial_variables ()) 

let functions : fn_binding list SymbolMap.t ref 
    = ref SymbolMap.empty

let add_new name value map =
  try
    let bindings = SymbolMap.find name !map
    in map := SymbolMap.add name (value::bindings) !map
  with Not_found ->
    map := SymbolMap.add name [value] !map


let remove_last name map =
  try 
    let bindings = SymbolMap.find name !map
    in map := SymbolMap.add name (List.tl bindings) !map
  with Not_found -> raise (Error "Trying to remove binding when none exist")

    
let get_current name map =
  let bindings = SymbolMap.find name !map in
  match bindings with
    [] -> raise Not_found
  | x::_ -> x



let lookup_variable name =
  try
    get_current name variables
  with 
    Not_found -> raise (Error ("Symbol's definition as variable is void: " ^ name))



let lookup_function name =
  try
    get_current name functions
  with
    Not_found -> raise (Error ("Symbol's definition as function is void" ^ name))


let add_variable_binding name defn =
  if name = "t" || name = "nil"
  then raise (Error "Trying to rebind constant");
  prerr_endline ("Adding binding for " ^ name ^ " of " ^ Asyntax.string_of_sexp defn);
  add_new name defn variables


let add_function_binding name defn =
  add_new name defn functions


let remove_variable_binding name =
  remove_last name variables

