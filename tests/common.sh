test_dir="tests"
zoo_dir="zoo"

ocamlopt="ocamlopt -stop-after typing -bin-annot -I $zoo_dir"
ocaml2zoo="./bin/ocaml2zoo.exe --force"

$ocamlopt "$zoo_dir/zoo.mli" "$zoo_dir/zoo.ml"
