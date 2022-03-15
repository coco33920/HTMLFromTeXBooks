latex-documentation:
	ocamldoc -t "HTMLFromTeXBooks Documentation" -d docs/ -latex lib/*.ml
	pdflatex ocamldoc.out 
	mv ocamldoc.pdf docs/latex/doc.pdf 
	rm ocamldoc.aux ocamldoc.log ocamldoc.out ocamldoc.sty ocamldoc.toc
html-documentation:
	dune build @doc-private
	chmod -R 776 _build/default/_doc/*
	cp -a _build/default/_doc/_html/. docs/
	mv docs/htmlfromtexbooks*/H*/Lib/ docs/htmlfromtexbooks
	rm -r docs/htmlfromtexbooks@*
	sed 's=../../../odoc.css=../odoc.css=' docs/htmlfromtexbooks/index.html > t.temp && rm docs/htmlfromtexbooks/index.html && mv t.temp docs/htmlfromtexbooks/index.html
	dune clean
documentation:
	dune clean
	rm -r docs/ 2> /dev/null
	mkdir docs/
	mkdir docs/latex
	make latex-documentation
	make html-documentation
.IGNORE: documentation