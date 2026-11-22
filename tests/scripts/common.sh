test_dir="tests"
zoo_dir="zoo"

ocamlopt="ocamlopt -stop-after typing -bin-annot -I ${zoo_dir}"
ocaml2zoo="./bin/ocaml2zoo.exe --force"

failwith () {
  echo "${1}"
  exit 1
}

error () {
  failwith "error: ${1}"
}

test_generated () {
  echo "${1}__${2}.v"
}
test_expected () {
  echo "${1}__${2}.exp"
}

test_has_interface () {
  [ -f "${1}.mli" ]
}
test_has_implementation () {
  [ -f "${1}.ml" ]
}

test_check () {
	if ! test_has_interface "${1}" && ! test_has_implementation "${1}" ; then
		error "test does not exist: ${1}"
	fi
}

test_generate () {
  if test_has_interface "${1}" ; then
    ${ocamlopt} "${1}.mli"
    ${ocaml2zoo} "${1}.cmti" "${test_dir}"
  fi
  if test_has_implementation "${1}" ; then
    if test_has_interface "${1}" ; then
      ${ocamlopt} -I "${test_dir}" "${1}.ml"
    else
      ${ocamlopt} "${1}.ml"
    fi
    ${ocaml2zoo} "${1}.cmt" "${test_dir}"
  fi
}

test_diff_aux () {
  local generated="$(test_generated "${1}" "${2}")"
  local expected="$(test_expected "${1}" "${2}")"

	if [ ! -f "${expected}" ] ; then
    return 1
	fi

  diff "${generated}" "${expected}" > /dev/null
}
test_diff () {
  test_diff_aux "${1}" "types" && \
  test_diff_aux "${1}" "code" && \
  test_diff_aux "${1}" "opaque"
}

test_copy_aux () {
  local generated="$(test_generated "${1}" "${2}")"
  local expected="$(test_expected "${1}" "${2}")"

  cp "${generated}" "${expected}"
}
test_copy () {
  test_copy_aux "${1}" "types"
  test_copy_aux "${1}" "code"
  test_copy_aux "${1}" "opaque"
}

${ocamlopt} "${zoo_dir}/zoo.mli" "${zoo_dir}/zoo.ml"

if [[ 0 < $# ]] ; then
	tests="$@"
	tests="${tests/#/${test_dir}/}"
else
  tests="$(ls ${test_dir}/*.ml{,i} | cut -d . -f 1 | sort -u)"
fi
