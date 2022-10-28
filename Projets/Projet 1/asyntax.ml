(*
Fichier asyntax.ml
Définition du typage de l'arbre syntaxique, et fonctions de vériﬁcation de type.
*)

type exp =
  | Int of int
  | Float of float
  | ToInt of exp
  | ToFloat of exp
  | Add of exp * exp
  | Addf of exp * exp
  | Sub of exp * exp
  | Subf of exp * exp
  | Mul of exp * exp
  | Mulf of exp * exp
  | Div of exp * exp
  | Mod of exp * exp
  | USub of exp
  | USubf of exp;;

type iORf = I | F;;
exception BadType;;
(* Cette fonction permet de vérifier que l'AST a seulement des 
opérations entre entiers et des variables de type entier *)
let rec check_ast_int a =
  match a with
  | Int _ -> true
  | ToInt a -> check_ast_float a
  | Add (a1, a2) -> check_ast_int a1 && check_ast_int a2
  | Sub (a1, a2) -> check_ast_int a1 && check_ast_int a2
  | Mul (a1, a2) -> check_ast_int a1 && check_ast_int a2
  | Div (a1, a2) -> check_ast_int a1 && check_ast_int a2
  | Mod (a1, a2) -> check_ast_int a1 && check_ast_int a2
  | USub a -> check_ast_int a
  | _ -> false

(* Cette fonction permet de vérifier que l'AST a seulement des
opérations entre flottants et des variables de type flottant *)
and check_ast_float a =
  match a with
  | Float _ -> true
  | ToFloat a -> check_ast_int a
  | Addf (a1, a2) -> check_ast_float a1 && check_ast_float a2
  | Subf (a1, a2) -> check_ast_float a1 && check_ast_float a2
  | Mulf (a1, a2) -> check_ast_float a1 && check_ast_float a2
  | USubf a -> check_ast_float a
  | _ -> false;;

let check_ast a =
  if check_ast_int a then I else if check_ast_float a then F else raise BadType;;

