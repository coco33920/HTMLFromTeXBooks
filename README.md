# HTMLFromTeXBooks
A simple command-line tools to generate HTML code from TeX files.

## What is this project
Welcome to this little project, I decided to make this because I like OCaml and the previous version of it was in Python. This program
translates TeX Books (simple one like stories kind of book) to HTML. I use this to publish the HTML version of [agh](https://agh.nwa2coco.fr) and to publish chapters written in TeX on ScribbleHub.

## What this project *is not*
The purpose of this project **is not** and never will be a kind of "TeX To HTML" compiler, I do *not* intend to implement the full TeX
and LaTeX specification. As such the support of mathematics is likely **never to be** included here. This is a tool mainly done *for me* for my usage, that is publishing my book on ScribbleHub and the web. I'm sure you can find real compilers if you want to support 
the full specification of TeX and LaTeX

## Support
The project currently translate *LaTeX* to human-readable HTML files (the line breaks in the HTML follows the line breaks in the TeX file while line breaks on HTML is let to the browser.) and supports the following features:

- Parsing chapter by chapter
- Printing chapter
- LaTeX `center` environment
- LaTeX macros
  - `textit` is replaced by `<i>`
  - `textbf` is replaced by `<b>`
  -  `newline`, `\\`, and `par` puts a `<br>`
  - `bigskip` closes the paragraph and open another one.
  
## Near Future TODO List
* parsing glossary entries from a glossary file and printing the `gls` with a link to the glossary 
* reference
* supports for section,subsection and subsubsection (`<h3>`,`<h4>` and `<b>` for books and `<h2>` `<h3>` `<h4>` for articles)

If you have an idea post an ISSUE, any contribution is welcomed :)
Let make no one ever forgets the name ~~Enter..~~ OCAML :)!

## Installing
```bash
git clone git@github.com:coco33920/HTMLFromTeXBooks.git
dune build @install
dune install
```

## Usage
WILL COME