failwith () {
  echo "$1"
  exit 1
}

error () {
  failwith "error: $1"
}

test_diff () {
	diff "${1}__types.v" "${1}__types.exp" > /dev/null && \
  diff "${1}__code.v" "${1}__code.exp" > /dev/null && \
  diff "${1}__opaque.v" "${1}__opaque.exp" > /dev/null
}

test_copy () {
	cp "${1}__types.v" "${1}__types.exp"
	cp "${1}__code.v" "${1}__code.exp"
	cp "${1}__opaque.v" "${1}__opaque.exp"
}

test_dir="tests"
zoo_dir="zoo"

ocamlopt="ocamlopt -stop-after typing -bin-annot -I ${zoo_dir}"
ocaml2zoo="./bin/ocaml2zoo.exe --force"

${ocamlopt} "${zoo_dir}/zoo.mli" "${zoo_dir}/zoo.ml"

if [[ 0 < $# ]] ; then
	tests="$@"
	tests="${tests/#/${test_dir}/}"
	tests="${tests/%/.ml}"
else
  tests="$(ls ${test_dir}/*.ml)"
fi
