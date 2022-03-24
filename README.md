<h1 align="center">HTMLFromTeXBooks</h1>
<h3 align="center">A simple command-line tools to generate HTML code from TeX files.</h3>

<div align="center">
<a href="https://github.com/coco33920/HTMLFromTeXBooks" title="Go to GitHub repo"><img src="https://img.shields.io/static/v1?label=coco33920&message=HTMLFromTeXBooks&color=55cdfc&logo=github&style=for-the-badge" alt="coco33920 - HTMLFromTeXBooks"></a>
<a href="https://github.com/coco33920/HTMLFromTeXBooks/releases/latest/"><img src="https://img.shields.io/github/release/coco33920/HTMLFromTeXBooks?include_prereleases=&sort=semver&color=55cdfc&style=for-the-badge" alt="GitHub release"></a>
<a href="LICENSE"><img src="https://img.shields.io/badge/License-MIT-55cdfc?style=for-the-badge" alt="License - MIT"></a>

<a href="https://doc.nwa2coco.fr"><img src="https://img.shields.io/badge/View-Documentation-f7a8d8?style=for-the-badge" alt="View site - GH Pages"></a>
</div>


## What is this project
Welcome to this little project, I decided to make this because I like OCaml and the previous version of it was in Python. This program
translates TeX Books (simple one like stories kind of book) to HTML. I use this to publish the HTML version of [agh](https://agh.nwa2coco.fr) and to publish chapters written in TeX on ScribbleHub.

## What this project *is not*
The purpose of this project **is not** and never will be a kind of "TeX To HTML" compiler, I do *not* intend to implement the full TeX
and LaTeX specification. This is a tool mainly done *for me* for my usage, that is publishing my book on ScribbleHub and the web. I'm sure you can find real compilers if you want to support 
the full specification of TeX and LaTeX

## Usage
The tool automatically detect the first tex file which's name's not "glossary.tex", scrapes the name and eventual glossary
input from the preamble and throw out an html file so the easiest way to use it is just
```bash
htmlfromtexbooks
```
by default it only prints after the first (or the nth depending on the command line args) chapter was read you can change that 
behaviour with the `--write` argument
```bash
htmlfromtexbooks --write
```
A full usage breakdown is available [here](https://doc.nwa2coco.fr/usage.html)

## Install

### OPAM
You can install the version **v3.3.1** with opam 
```shell
opam install htmlfromtexbooks
```
it installs the executable under `htmlfromtexbooks` and the library under `htmlfromtexbooks.lib`

### Downloading last stable from release
The **Automatic Script** install is available here, just type this command and the `htmlfromtexbooks` is installed under `~/.local/bin`
```shell
curl https://raw.githubusercontent.com/coco33920/HTMLFromTeXBooks/master/downloading.sh | sh
```

Alternatively you can download the script then run it if you want to read it before executing it
```shell
wget https://raw.githubusercontent.com/coco33920/HTMLFromTeXBooks/master/downloading.sh
sh downloading.sh
```

Or executing the individual commands themselves
```shell 
wget https://github.com/coco33920/HTMLFromTeXBooks/releases/latest/download/htmlfromtexbooks
mv htmlfromtexbooks ~/.local/bin/htmlfromtexbooks
chmod +x ~/.local/bin/htmlfromtexbooks
```

### Building from source
The script to automatically build from sources 
```shell
curl https://raw.githubusercontent.com/coco33920/HTMLFromTeXBooks/master/building.sh | sh
```  

It builds it with dune and install it under the OPAM path with the name `htmlfromtexbooks`, which 
performs these commands. It also installs the library under the name `htmlfromtexbooks.lib`
```shell
git clone https://github.com/coco33920/HTMLFromTeXBooks
cd HTMLFromTeXBooks
dune build @install 
dune install
```

## Support
The project currently translate *LaTeX* to human-readable HTML files (the line breaks in the HTML follows the line breaks in the TeX file while line breaks on HTML is let to the browser.) and supports the following features:

- Parsing chapter by chapter
- Automatically detect a tex file, generate a name for the output, detects the glossary and use the configuration file for name/starting chapter
- Extracting the title of the tex file for name
- Printing chapter
- LaTeX `center` environment
- LaTeX macros
  - `textit` is replaced by `<i>`
  - `textbf` is replaced by `<b>`
  -  `newline`, `\\`, and `par` puts a `<br>`
  - `bigskip` closes the paragraph and open another one.
- Some math
  - Inline math with `$`
  - Math env 
    - Align*
    - Align
    - Equation
    - Equation*
  
Math image are generated with the `latex.codecogs.com` backend an example bellow with `$\int^b_a f(x) \mathrm(d)x$`

![t](https://latex.codecogs.com/svg.image?\begin{equation}\int^b_af(x)\mathrm(d)x\end{equation})  

  
## Near Future TODO List
* parsing glossary entries from a glossary file and printing the `gls` with a link to the glossary 
* reference
* supports for section,subsection and subsubsection (`<h3>`,`<h4>` and `<b>` for books and `<h2>` `<h3>` `<h4>` for articles)

If you have an idea post an ISSUE, any contribution is welcomed :)
Let make no one ever forgets the name ~~Enter..~~ OCAML :)!

## Installing
Be sure to have dune installed and OCaml version >=4.08
```bash
ocaml --version #must be >= 4.08
```
```bash
opam install dune
```
and
```bash
git clone git@github.com:coco33920/HTMLFromTeXBooks.git
dune build @install
dune install
```

## Usage
WILL COME
