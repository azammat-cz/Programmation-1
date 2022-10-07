all: test_my_list rapport.pdf
	ocamlc test_my_list.ml -o test_my_list
	pdflatex -shell-escape rapport.tex

test_my_list:
	ocamlc test_my_list.ml -o test_my_list

rapport.pdf:
	pdflatex -shell-escape rapport.tex	

clean:
	rm -rf test_my_list *.cmi *.cmo *~
