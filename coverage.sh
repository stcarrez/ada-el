#!/bin/sh
NAME=elada.cov
lcov --base-directory . --directory . -c --include "*/ada-el/src/*" -o $NAME
rm -rf cover
genhtml --ignore-errors source -o ./cover -t "test coverage" --num-spaces 4 $NAME
 
