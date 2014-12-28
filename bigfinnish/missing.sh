#!/bin/bash

for f in $( ls x* ); do
	if [ ! -e "freqdist_$f" ]; then
		create_fd $f	
	fi
done
