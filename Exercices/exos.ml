let list_to_array liste = match liste with
  | [] -> [||]
  | t::q -> let ans = Array.make (List.length liste) t in let i = ref 0 in List.iter (function x -> ans.(!i) <- x; incr i) liste; ans;;
let tab = list_to_array [1;2;3];;
(* for i = 0 to (Array.length tab) -1 do
    print_int tab.(i);
    print_string "\n";
    done; *)

let array_to_list tab =
  let liste = ref [] in
  for i = (Array.length tab)-1 downto 0 do
    liste:=tab.(i)::!liste;
  done;
  !liste;;

array_to_list [|1;2;3|];;

let make_matrix n m =
  let ans = Array.make n [||] in
  for i = 0 to n-1 do
    ans.(i) <- Array.make m 0
  done;
  ans;;

