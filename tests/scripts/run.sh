#! /usr/bin/env bash

set -eou pipefail

. tests/scripts/common.sh

for test in ${tests} ; do
  test_check "${test}"

	test_generate "${test}"

	if ! test_has_implementation "${test}" || test_diff "${test}" ; then
		echo "test successful: $(basename ${test})"
	else
		failwith "test failed: $(basename ${test})"
	fi
done
