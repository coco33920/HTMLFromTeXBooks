(lang dune 2.9)

(name htmlfromtexbooks)

(generate_opam_files true)

(source
 (github coco33920/HTMLFromTeXBooks))

(authors "Charlotte THOMAS")

(maintainers "Charlotte THOMAS")

(license MIT)

(package
 (name htmlfromtexbooks)
 (synopsis "From TeX To Human-Readable HTML")
 (description "A simple command line tool to generate human-readable HTML from a LaTeX Book/Article")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
