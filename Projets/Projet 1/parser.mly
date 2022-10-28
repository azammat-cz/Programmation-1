/*
Fichier parser.mly
Nous définissons la grammaire du parser.
*/
%{open Asyntax%}
        %token <int> INT
        %token <float> FLOAT
        %token TOINT
        %token TOFLOAT
        %token PLUS PLUSF MINUS MINUSF MULT MULTF DIV MOD
        %token LPAREN RPAREN
        %token EOL /* Fin de ligne */
        %left PLUS MINUS
        %left MULT DIV MOD
        %nonassoc UMINUS UMINUSF
        %start main
        %type <Asyntax.exp> main
        %%
        main:
            expr EOL                { $1 }
        ;
        expr:
          | LPAREN expr RPAREN      { $2 }
          | INT                     { Int($1) }
          | FLOAT                   { Float($1) }
          | TOINT expr              { ToInt($2) }
          | TOFLOAT expr            { ToFloat($2) }
          | expr PLUSF expr         { Addf($1, $3) }
          | expr PLUS expr          { Add($1, $3) }
          | expr MINUSF expr        { Subf($1, $3) }
          | expr MINUS expr         { Sub($1, $3) }
          | expr MULTF expr         { Mulf($1, $3) }
          | expr MULT expr          { Mul($1, $3) }
          | expr DIV expr           { Div($1, $3) }
          | expr MOD expr           { Mod($1, $3) }
          | MINUSF expr %prec UMINUSF { USubf($2) }
          | MINUS expr %prec UMINUS { USub($2) }
        ;