(lang dune 2.9)
(version 3.3.2)

(name htmlfromtexbooks)

(generate_opam_files false)

(source
 (github coco33920/HTMLFromTeXBooks))
(authors "Charlotte THOMAS")

(maintainers "Charlotte THOMAS")

(license MIT)

(package
 (name htmlfromtexbooks)
 (synopsis "From TeX To Human-Readable HTML")
 (description "A simple command line tool to generate human-readable HTML from a LaTeX Book")
 (depends ocaml dune))
