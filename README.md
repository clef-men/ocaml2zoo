## Synopsis

`ocaml2zoo` is a tool to translate OCaml programs into [Zoo](https://github.com/clef-men/zoo), a deeply embedded language living inside [Coq](https://coq.inria.fr/).

To translate the [`dune`](https://dune.build/) project living in the `proj` directory to the `dst` directory (where Coq files are generated), run:

```
ocaml2zoo proj dst
```

## Building

First, you need to install [`opam`](https://opam.ocaml.org/) (>= 2.0).

To make sure it is up-to-date, run:

```
opam update --all --repositories
```

Then, you need to install [this custom version of the OCaml compiler](https://github.com/clef-men/ocaml/tree/atomic_fields_11) featuring atomic record fields.
Hopefully, the corresponding PR should be merged one day.

The following commands take care of this:

```
opam switch create . --empty
eval $(opam env --switch=. --set-switch)
opam pin add ocaml-variants git+https://github.com/clef-men/ocaml#atomic_fields_11 --yes
```

Then, install dependencies with:

```
opam install . --deps-only --yes
```

Finally, to compile `ocaml2zoo`, run:

```
make
```
