#!/bin/bash

for f in $*
do
    sed "s/\([aeiouy]\+\)h/\\1\\1/g" "$f" -i
done
