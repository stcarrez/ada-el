#!/bin/sh
NAME=elada.cov
alr exec -- lcov --quiet --base-directory . --directory . \
   --no-external \
   --exclude '*/b__*.adb' \
   --exclude '*/<unknown>' \
   --exclude '*/samples/*' \
   --exclude '*/regtests*'  -c -o $NAME
rm -rf cover
genhtml --quiet --ignore-errors source -o ./cover -t "test coverage" --num-spaces 4 $NAME
 
