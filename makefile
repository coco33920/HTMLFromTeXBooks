html-documentation:
	dune build @doc-private
	chmod -R 776 _build/default/_doc/*
	cp -a _build/default/_doc/_html/. docs/
	mv docs/htmlfromtexbooks*/H* docs/htmlfromtexbooks
	rm -r docs/htmlfromtexbooks@*
	sed 's=../../odoc.css=../odoc.css=' docs/htmlfromtexbooks/index.html > test.u && mv test.u docs/htmlfromtexbooks/index.html
	sed 's=../../../odoc.css=../../odoc.css=' docs/htmlfromtexbooks/Utils/index.html > test.u && mv test.u docs/htmlfromtexbooks/Utils/index.html
	sed 's=../../../odoc.css=../../odoc.css=' docs/htmlfromtexbooks/Parser/index.html > test.u && mv test.u docs/htmlfromtexbooks/Parser/index.html
	sed 's=../../../odoc.css=../../odoc.css=' docs/htmlfromtexbooks/Glossary/index.html > test.u && mv test.u docs/htmlfromtexbooks/Glossary/index.html

	sed 's=../../highlight.pack.js=../highlight.pack.js=' docs/htmlfromtexbooks/index.html > temp.u && mv temp.u docs/htmlfromtexbooks/index.html
	sed 's=../../../highlight.pack.js=../../highlight.pack.js=' docs/htmlfromtexbooks/Utils/index.html > temp.u && mv temp.u docs/htmlfromtexbooks/Utils/index.html
	sed 's=../../../highlight.pack.js=../../highlight.pack.js.css=' docs/htmlfromtexbooks/Parser/index.html > temp.u && mv temp.u docs/htmlfromtexbooks/Parser/index.html
	sed 's=../../../highlight.pack.js=../../highlight.pack.js.css=' docs/htmlfromtexbooks/Glossary/index.html > temp.u && mv temp.u docs/htmlfromtexbooks/Glossary/index.html
	dune clean
documentation:
	dune clean
	rm -r docs/ 2> /dev/null
	mkdir docs/
	make html-documentation
.IGNORE: documentation