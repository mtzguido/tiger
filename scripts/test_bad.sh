#!/bin/bash

for i in tests/bad/*.tig; do
	if ./tiger "$i" &>/dev/null; then
		echo "Test $i FALLADO!" ;
		exit 1 ;
	else
		echo -n "." ;
	fi
done

echo -e '\nall good'
