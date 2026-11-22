#! /usr/bin/env bash

set -eou pipefail

. tests/scripts/common.sh

for test in ${tests} ; do
  test_check "${test}"

	test_generate "${test}"

	if test_has_implementation "${test}" && ! test_diff "${test}" ; then
    test_copy "${test}"
    echo "test promoted: $(basename ${test})"
	fi
done
