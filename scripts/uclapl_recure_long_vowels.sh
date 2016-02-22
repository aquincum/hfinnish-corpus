#!/bin/bash

for f in $*
do
    sed "s/\([aeiou]\+\)h/\\1\\1/g" "$f" -i
done
