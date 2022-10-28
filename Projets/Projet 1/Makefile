all: aritha rapport

aritha: 
	ocamlyacc parser.mly
	ocamllex lexer.mll
	ocamlc x86_64.mli x86_64.ml asyntax.ml parser.mli parser.ml lexer.ml compiler.ml main.ml -o aritha

rapport:
	pdflatex -shell-escape rapport.tex

clean:
	rm -rf *.cmi *.cmx *.o *.s *.cmo *.pdf *.aux *.out *.log parser.ml parser.mli lexer.ml aritha
