#!/usr/bin/env bash

make clean --always-make && make tests --always-make

tbold=$(tput bold)
tnormal=$(tput sgr0)
tred=$(tput setaf 1)
tgreenish=$(tput setaf 6)
failed=$((0))
passed=$((0))

for f in tests/*.out; do
    testcase=${f%.*}
    expect="${testcase}.expect"
    echo -ne "Test Case '${expect}' : "
    if cmp -s ${f} ${expect}; then
        echo -e "${tbold}${tgreenish}[PASSED]${tnormal}"
        passed=$((passed + 1))
    else    
        echo "${tbold}${tred}[FAILED]${tnormal}";
        echo "With input poly: $(cat ${testcase}.in)" 
        git diff --no-index ${f} ${expect}
        failed=$((failed + 1))
    fi
done

echo -e "${tbold}Test Summary: $passed Passed and $failed Failed"