type 'a my_list = 
	| Nil
	| Cons of 'a * 'a my_list;;

(*** FONCTIONS ***)

let string_of_list str_fun l = 
	let rec string_content = function
    	| Nil  -> ""
    	| Cons(x,Nil)  -> (str_fun x)
    	| Cons(x,l) -> (str_fun x) ^ ", " ^ (string_content l) 
  	in "[" ^ (string_content l) ^ "]";;


let hd l = match l with
	| Nil -> failwith "Liste vide"
	| Cons(x,_) -> x;;


let tl l = match l with
	| Nil -> Nil
	| Cons(_,q) -> q;;


let rec length l = match l with
	| Nil -> 0
	| Cons(_,q) -> 1 + length q;;


let rec map f l = match l with
	| Nil -> Nil
        | Cons(x,q) -> Cons(f x, map f q);;

let string_of_nat_list = string_of_list string_of_int;;
let string_of_string_list = string_of_list (fun x -> x);;

(*** TESTS ***)

let empty = Nil;;
let one = Cons("a",Nil);;
let lst = Cons(1,Cons(3,Cons(5,Cons(10,Cons(15,Cons(21,Cons(28,Cons(36,Cons(45,Cons(55,Nil))))))))));;

let test_hd () = 
        Printf.printf "Tête de %s : %s.\n" (string_of_string_list one) (hd one);
        Printf.printf "Tête de %s : %d.\n\n" (string_of_nat_list lst) (hd lst);;

let test_tl () = 
        Printf.printf "Queue de %s : %s.\n" (string_of_string_list one) (string_of_string_list (tl one));
        Printf.printf "Queue de %s : %s.\n\n" (string_of_nat_list lst) (string_of_nat_list (tl lst));;

let test_length () = 
        Printf.printf "Taille de %s : %d.\n" (string_of_string_list one) (length one);
        Printf.printf "Taille de %s : %d.\n" (string_of_nat_list lst) (length lst);
        Printf.printf "Taille de %s : %d.\n\n" (string_of_string_list empty) (length empty);;

let test_map ()= 
        Printf.printf "Map de (x -> xx) sur %s : %s.\n" (string_of_string_list one) (string_of_string_list (map (fun s -> s ^ s) one));
        Printf.printf "Map de (x -> 2x) sur %s : %s.\n" (string_of_nat_list lst) (string_of_nat_list (map (fun n -> 2 * n) lst));
        Printf.printf "Map de (x -> 2x) sur %s : %s.\n\n" (string_of_nat_list empty) (string_of_nat_list (map (fun n -> 2 * n) empty));;

let main () =
        test_hd(); test_tl(); test_length(); test_map();;

main();;
