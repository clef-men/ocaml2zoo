#! /usr/bin/env bash

set -eou pipefail

. tests/common.sh

for test in ${tests} ; do
	if [ ! -f "${test}" ] ; then
		error "test does not exist: ${test}"
	fi

	test="${test%.*}"

	${ocamlopt} "${test}.ml"
	${ocaml2zoo} "${test}.cmt" "${test_dir}"

	if ! test_diff "${test}" ; then
    test_copy "${test}"
    echo "test promoted: $(basename ${test})"
	fi
done
