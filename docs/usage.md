# TeX to HTML Translation
As of 15th of March 2022 I switch from the python script to a more robust one created in OCaml. 
The repo is [here](https://github.com/coco33920/HTMLFromTeXBooks). To install it:

```bash
git clone git@github.com:coco33920/HTMLFromTeXBooks.git
dune build @install
dune install
htmlfromtexbooks --help
```

Or download the binaries for your system (only linux 64 bits is pre compiled)

## New Usage
See [legacy](#legacy-usage) for the command line version

This version of the program supports automatically extracting information, so if you type only
```bash
htmlfromtexbooks
```
The program will:
* Search in the current directory for a file with the `.tex` extension
  * If it fails (there is not) the program exit with code 2
  * If it succeed it takes the first tex file *that is not named `glossary.tex`* and uses it as the input file
* Generate a outname by switching the `.tex` by `.html`
* Scan the preamble of the document
  * If it sees a \input{glossary.tex} it assumes that `file.tex` is serving the glossary
  * It scan for \title{} and use that value as name
* Use 1 as starting chapter, the number (and the default name in the case there isn't a title) is configurable in the ~/.htmlfromtex/.config file
* And use it 

It's equivalent to 
```bash
htmlfromtexbooks --input file.tex --output file.html --use-glossary glossary.tex --name <title> --start-chapter <configuration>
```

## Legacy Usage
To smooth the transition from the python script the scripts recognize the exact same structure
The TeX to HTML translation is done with the command `htmlfromtexbooks` (you can alternatively copy the file under 
you own command folder and rename it as you want). 

### Basic Usage. 
The basic usage of the command is the following:
```bash
htmlfromtexbooks --input <input_file> --output <output_file>
```
example
```bash
htmlfromtexbooks --input agh.tex --output agh.html
```
This is the only two required arguments, this command will translate the `agh.tex` file into HTML and 
prints it in the `agh.html` file. The name displayed in `<title>` and `<h1>` of the file will 
be the default one (which is "TeX"). To change it you can use the `--name "A Name"` option

```bash
htmlfromtexbooks --input agh.tex --output agh.html --name "A Galactic HRT"
```
This will put the name "A Galactic HRT" in the correct places.

### Glossary
The glossary is read from an `\input{}` command on LaTeX
example
```bash 
htmlfromtexbooks --input agh.tex --name "A Galactic HRT" --output agh.html
```
Will use the `glossary.tex` file to provide entries to parse `\gls{}` commands. Use the 
name "A Galactic HRT" and prints the result to the agh.html file

### Chapter
You can choose a specific chapter to print with the `--chapter <chapter>` option
example
```bash
htmlfromtexbooks --input agh.tex --use-glossary glossary.tex --chapter 2 --output chap2.html
```

### Starting point
You can change at which chapter the HTML parsing starts with the `--start-chapter <chapter>` (default: 1) option 
for example for AGH this option is `2` because I don't want the Edito to be parsed. 0 is before the first chapter,
1 is the edito and 2 is the prologue, which i want.
```bash
htmlfromtexbooks --input agh.tex --output agh.html --start-chapter 2
```

### AGH
The full command line compilation for agh/agh-current/agh-french is
```bash
htmlfromtexbooks --input agh.tex --output web/agh.html --start-chapter 2
```