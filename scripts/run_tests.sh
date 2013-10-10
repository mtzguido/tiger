#!/bin/bash

for i in tests/bad/*.tig; do
	if ./tiger "$i" &>/dev/null; then
		echo
		echo "Test $i FAILED" ;
		exit 1 ;
	else
		echo -n "." ;
	fi
done
for i in tests/good/*.tig; do
	if ./tiger "$i" &>/dev/null; then
		echo -n "." ;
	else
		echo
		echo "Test $i FAILED" ;
		exit 1 ;
	fi
done

echo -e '\nAll tests OK'
