#!/bin/bash

for i in tests/bad/*.tig; do
	if ./dtc "$i" &>/dev/null; then
		echo -e "\nTest $i FAILED" ;
		exit 1 ;
	else
		echo -n "." ;
	fi
done
for i in tests/good/*.tig; do
	if ./dtc "$i" &>/dev/null; then
		echo -n "." ;
	else
		echo -e "\nTest $i FAILED" ;
		exit 1 ;
	fi
done

echo -e '\nall tests OK'
