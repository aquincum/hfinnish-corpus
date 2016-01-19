#!/bin/bash

for f in $( ls freqdist_x* ); do
	if [ ! -e "filtered2_$f" ]; then
		filter_fds -s $f
	fi
done
