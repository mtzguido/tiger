#!/bin/bash

for i in tests/good/*.tig; do
	if ./tiger "$i" &>/dev/null; then
		echo -n "." ;
	else
		echo "Test $i FALLADO!" ;
		exit 1 ;
	fi
done
echo -e "\nall good"
