#!/bin/bash

if [ $# != 1 ]; then
    echo "usage: test.sh (nb|lam|pcf)"
    exit 1
fi

case $1 in
    nb) TESTS=nb ;;
    lam) TESTS="nb lam";;
    pcf) TESTS="nb pcf";;
    *) echo "usage: test.sh (nb|lam|pcf)"
       exit 1;;
esac

cd "$(dirname "$0")"
cd ..

OS=$(uname | tr A-Z a-z)

PASS=1

for TEST in $TESTS; do

    for FILE in test/diff.py solution/$TEST.$OS submit/$TEST ; do
        if [ ! -x $FILE ]; then
	    echo "error: $FILE missing or not executable"
	    exit 1
        fi
    done
    
    if [ ! -f test/$TEST.input ]; then
        echo "error: $TEST.input missing or not readable"
        exit 1
    fi
    
    if ! test/diff.py test/$TEST.input <(solution/$TEST.$OS <test/$TEST.input) <(submit/$TEST <test/$TEST.input); then
        PASS=0
    fi
done

if [ "$PASS" = 1 ]; then
    echo "All tests passed!"
else
    echo "Some tests failed."
fi
