(lang dune 3.0)

(name ocaml2zoo)

(generate_opam_files)

(authors
  "Clément Allain"
)
(maintainers
  "clement.allain@inria.fr"
)

(source (github clef-men/ocaml2zoo))

(package
  (name ocaml2zoo)
  (synopsis "OCaml to Zoo")
  (depends
    ocaml-compiler-libs
    csexp
    fmt
    cmdliner
  )
)
