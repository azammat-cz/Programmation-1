type token =
  | NAME of (string)
  | LBRACE
  | RBRACE
  | EOF
  | QUOTE
  | DOT

open Parsing;;
let _ = parse_error;;
let yytransl_const = [|
  258 (* LBRACE *);
  259 (* RBRACE *);
    0 (* EOF *);
  260 (* QUOTE *);
  261 (* DOT *);
    0|]

let yytransl_block = [|
  257 (* NAME *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\005\000\005\000\
\005\000\004\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\002\000\002\000\003\000\003\000\001\000\
\002\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\010\000\000\000\000\000\011\000\001\000\002\000\
\003\000\005\000\000\000\000\000\004\000\000\000\009\000\006\000\
\007\000"

let yydgoto = "\002\000\
\006\000\011\000\008\000\009\000\012\000"

let yysindex = "\014\000\
\010\255\000\000\000\000\006\255\010\255\000\000\000\000\000\000\
\000\000\000\000\001\255\254\254\000\000\010\255\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\013\255\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\255\255\000\000\000\000\006\000"

let yytablesize = 17
let yytable = "\007\000\
\016\000\003\000\004\000\013\000\005\000\014\000\003\000\004\000\
\010\000\005\000\003\000\004\000\017\000\005\000\001\000\008\000\
\015\000"

let yycheck = "\001\000\
\003\001\001\001\002\001\005\000\004\001\005\001\001\001\002\001\
\003\001\004\001\001\001\002\001\014\000\004\001\001\000\003\001\
\011\000"

let yynames_const = "\
  LBRACE\000\
  RBRACE\000\
  EOF\000\
  QUOTE\000\
  DOT\000\
  "

let yynames_block = "\
  NAME\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sexp) in
    Obj.repr(
# 19 "parser.mly"
         ( _1 )
# 83 "parser.ml"
               : Asyntax.sexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 23 "parser.mly"
       ( _1 )
# 90 "parser.ml"
               : 'sexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 24 "parser.mly"
       ( _1 )
# 97 "parser.ml"
               : 'sexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sexp) in
    Obj.repr(
# 25 "parser.mly"
             ( Asyntax.Cons (Asyntax.Atom "quote", Asyntax.Cons(_2, Asyntax.Atom "nil")) )
# 104 "parser.ml"
               : 'sexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 29 "parser.mly"
                ( Asyntax.Atom "nil" )
# 110 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'inside_list) in
    Obj.repr(
# 30 "parser.mly"
                            ( _2 )
# 117 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'sexp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sexp) in
    Obj.repr(
# 34 "parser.mly"
                ( Asyntax.Cons (_1,_3) )
# 125 "parser.ml"
               : 'inside_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sexp) in
    Obj.repr(
# 35 "parser.mly"
       ( Asyntax.Cons (_1, Asyntax.Atom "nil") )
# 132 "parser.ml"
               : 'inside_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'sexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'inside_list) in
    Obj.repr(
# 36 "parser.mly"
                   (Asyntax.Cons(_1,_2))
# 140 "parser.ml"
               : 'inside_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 39 "parser.mly"
           ( Asyntax.Atom _1 )
# 147 "parser.ml"
               : 'atom))
(* Entry parse *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let parse (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Asyntax.sexp)
