#!/bin/bash

for i in ../tests/good/*.tig; do
	echo -n "$i:"
	./tiger "$i"
done
