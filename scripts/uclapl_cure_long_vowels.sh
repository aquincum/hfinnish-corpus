#!/bin/bash

for f in $*
do
    sed "s/\([aeiou]\+\)\\1/\\1h/g" "$f" -i
done
