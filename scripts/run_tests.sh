#!/bin/bash

for i in tests/bad/*.tig; do
	if ! ./dtc "$i" | grep '^COMPILATION FAILED$' &>/dev/null ; then
		echo -e "\nTest $i FAILED" ;
		exit 1 ;
	else
		echo -n "." ;
	fi
done
for i in tests/good/*.tig; do
	if ! ./dtc "$i" | grep '^COMPILATION OK$' &>/dev/null; then
		echo -e "\nTest $i FAILED" ;
		exit 1 ;
	else
		echo -n "." ;
	fi
done

echo -e '\nall tests OK'
