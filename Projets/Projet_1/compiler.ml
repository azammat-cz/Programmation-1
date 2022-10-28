(*
Fichier compiler.ml
On lit l'AST et on renvoie le type program correspondant. Il sera utilisé dans le fichier main.ml pour produire expression.s.
*)

open Asyntax
open X86_64

(* pushq et popq version floattants *)
let pushf reg = movsd reg (ind ~ofs:(-8) rsp) ++ subq (imm 8) !%rsp
let popf reg = movsd (ind rsp) !%reg ++ addq (imm 8) !%rsp

let compile ast tp =
  let flottants = ref nop in (* Cette liste permet de stocker les éventuelles constantes flottantes dans des labels (apparemment nécessaire...) *)
  let i = ref 0 in (* Compteur associé pour attribuer un numéro d'apparition aux flottants *)
  let rec util a = match a with
  (* AST de type entier *)
  | Int(x) -> pushq (imm x)
  | Add(a1,a2) -> util a1 ++ util a2 ++ popq rax ++ popq rbx ++ addq !%rax !%rbx ++ pushq !%rbx
  | Sub(a1,a2) -> util a1 ++ util a2 ++ popq rax ++ popq rbx ++ subq !%rax !%rbx ++ pushq !%rbx
  | Mul(a1,a2) -> util a1 ++ util a2 ++ popq rax ++ popq rbx ++ imulq !%rax !%rbx ++ pushq !%rbx
  | Div(a1,a2) -> util a1 ++ util a2 ++ xorq !%rdx !%rdx ++ popq rbx ++ popq rax ++ idivq !%rbx ++ pushq !%rax
  | Mod(a1,a2) -> util a1 ++ util a2 ++ xorq !%rdx !%rdx ++ popq rbx ++ popq rax ++ idivq !%rbx ++ pushq !%rdx
  (* AST de type flottant *)
  | Float(x) -> begin
                  incr i;
                  flottants := !flottants ++ label (".LC" ^ (string_of_int !i)) ++ double x;
                  movsd !%(lc !i) !%xmm0;
                end
  | Addf(a1,a2) -> util a1 ++ pushf !%xmm0 ++ util a2 ++ popf xmm1 ++ addsd !%xmm1 !%xmm0
  | Subf(a1,a2) -> util a1 ++ pushf !%xmm0 ++ util a2 ++ movsd !%xmm0 !%xmm1 ++ popf xmm0 ++ subsd !%xmm1 !%xmm0
  | Mulf(a1,a2) -> util a1 ++ pushf !%xmm0 ++ util a2 ++ popf xmm1 ++ mulsd !%xmm1 !%xmm0
  (* Conversion flottant -> entier *)
  | ToInt(a) -> util a ++ cvttsd2si !%xmm0 !%rax ++ pushq !%rax
  (* Conversion entier -> flottant *)
  | ToFloat(a) -> util a ++ popq rax ++ cvtsi2sd !%rax !%xmm0
  (* Passage à l'opposé *)
  | USub(a) -> util a ++ popq rbx ++ negq !%rbx ++ pushq !%rbx
  | USubf(a) -> util (Mulf(Float(-1.),a))
  in let res = util ast in 
  if tp = I then
  (
  { text = globl "main" ++ label "main" ++ res ++ popq rax ++ movq !%rax !%rdi ++ call "print_int" ++ ret ++
  inline "
print_int:
  movq    %rdi, %rsi
  movq    $S_int, %rdi
  xorq    %rax, %rax
  call    printf
  ret

"; data = label "S_int" ++ string "%d" ++ !flottants})
  else (* tp = F *)
  (
  {text = globl "main" ++ label "main" ++ pushq !%rbp ++ res ++ call "print_float" ++ popq rbp ++ ret ++
  inline "
print_float:
  pushq   %rbp
  movq    %rsp, %rbp
  movl    $S_float, %edi
  movl    $1, %eax
  call    printf
  leave
  ret

"; data = label "S_float" ++ string "%f" ++ !flottants})
;;
