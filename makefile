html-documentation:
	dune build @doc
	chmod -R 776 _build/default/_doc/*
	cp -a _build/default/_doc/_html/. docs/doc/
	mv docs/doc/htmlfromtexbooks*/H* docs/doc/htmlfromtexbooks
	rm -r docs/doc/htmlfromtexbooks@*
	sed 's=../../odoc.css=../odoc.css=' docs/doc/htmlfromtexbooks/index.html > test.u && mv test.u docs/doc/htmlfromtexbooks/index.html
	sed 's=../../../odoc.css=../../odoc.css=' docs/doc/htmlfromtexbooks/Utils/index.html > test.u && mv test.u docs/doc/htmlfromtexbooks/Utils/index.html
	sed 's=../../../odoc.css=../../odoc.css=' docs/doc/htmlfromtexbooks/Parser/index.html > test.u && mv test.u docs/doc/htmlfromtexbooks/Parser/index.html
	sed 's=../../../odoc.css=../../odoc.css=' docs/doc/htmlfromtexbooks/Glossary/index.html > test.u && mv test.u docs/doc/htmlfromtexbooks/Glossary/index.html
	sed 's=../../../odoc.css=../../odoc.css=' docs/doc/htmlfromtexbooks/Htmlgen/index.html > test.u && mv test.u docs/doc/htmlfromtexbooks/Glossary/index.html
	sed 's=../../../odoc.css=../../odoc.css=' docs/doc/htmlfromtexbooks/Mathgen/index.html > test.u && mv test.u docs/doc/htmlfromtexbooks/Glossary/index.html

	sed 's=../../highlight.pack.js=../highlight.pack.js=' docs/doc/htmlfromtexbooks/index.html > temp.u && mv temp.u docs/doc/htmlfromtexbooks/index.html
	sed 's=../../../highlight.pack.js=../../highlight.pack.js=' docs/doc/htmlfromtexbooks/Utils/index.html > temp.u && mv temp.u docs/doc/htmlfromtexbooks/Utils/index.html
	sed 's=../../../highlight.pack.js=../../highlight.pack.js.css=' docs/doc/htmlfromtexbooks/Parser/index.html > temp.u && mv temp.u docs/doc/htmlfromtexbooks/Parser/index.html
	sed 's=../../../highlight.pack.js=../../highlight.pack.js.css=' docs/doc/htmlfromtexbooks/Glossary/index.html > temp.u && mv temp.u docs/doc/htmlfromtexbooks/Glossary/index.html
	sed 's=../../../highlight.pack.js=../../highlight.pack.js.css=' docs/doc/htmlfromtexbooks/Htmlgen/index.html > temp.u && mv temp.u docs/doc/htmlfromtexbooks/Htmlgen/index.html
	sed 's=../../../highlight.pack.js=../../highlight.pack.js.css=' docs/doc/htmlfromtexbooks/Mathgen/index.html > temp.u && mv temp.u docs/doc/htmlfromtexbooks/Mathgen/index.html
	dune clean
documentation:
	dune clean
	rm -r docs/doc/ 2> /dev/null
	mkdir docs/doc/
	make html-documentation
.IGNORE: documentation