(lang dune 3.13)

(using menhir 3.0)

(name ocaml2zoo)

(generate_opam_files)

(authors
 "Clément Allain"
)
(maintainers
 "clement.allain@cs.au.dk"
)

(source (github clef-men/ocaml2zoo))
(license ISC)

(package
 (name ocaml2zoo)
 (synopsis "OCaml to Zoo")
 (depends
  cmdliner
  csexp
  fmt
  menhir
  ocaml-compiler-libs
 )
)
