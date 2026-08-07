test_dir="tests"
zoo_dir="zoo"

ocamlopt="ocamlopt -stop-after typing -bin-annot -I $zoo_dir"
ocaml2zoo="./bin/ocaml2zoo.exe --force"

$ocamlopt "$zoo_dir/zoo.mli" "$zoo_dir/zoo.ml"

if [[ 0 < $# ]] ; then
	tests="$@"
	tests="${tests/#/$test_dir/}"
	tests="${tests/%/.ml}"
else
  tests="$(ls $test_dir/*.ml)"
fi
