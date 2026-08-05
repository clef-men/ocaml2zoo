#! /usr/bin/env bash

set -eou pipefail

. tests/common.sh

if [[ 0 < $# ]] ; then
	tests="$@"
	tests="${tests/#/$test_dir/}"
	tests="${tests/%/.ml}"
else
  tests="$(ls $test_dir/*.ml)"
fi

for test in $tests ; do
	test="${test%.*}"

	$ocamlopt "$test.ml"
	$ocaml2zoo "$test.cmt" "$test_dir"

	if diff "${test}__types.v" "${test}__types.exp" > /dev/null \
	&& diff "${test}__code.v" "${test}__code.exp" > /dev/null
	then
		echo "test successful: $(basename $test)"
	else
		echo "test failed: $(basename $test)"
		exit 1
	fi
done
