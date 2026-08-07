#! /usr/bin/env bash

set -eou pipefail

. tests/common.sh

for test in $tests ; do
	if [ ! -f "$test" ] ; then
		echo "error: test does not exist: $test"
		exit 1
	fi

	test="${test%.*}"

	$ocamlopt "$test.ml"
	$ocaml2zoo "$test.cmt" "$test_dir"

	cp "${test}__types.v" "${test}__types.exp"
	cp "${test}__code.v" "${test}__code.exp"
	cp "${test}__opaque.v" "${test}__opaque.exp"

  echo "test promoted: $(basename $test)"
done
