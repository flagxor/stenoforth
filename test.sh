#! /bin/bash

set -e

./out/stenoforth < tests/redefine.fs >/tmp/test.out 2>/dev/null
diff tests/redefine_expected.txt /tmp/test.out
