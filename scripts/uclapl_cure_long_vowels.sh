#!/bin/bash

for f in $*
do
    sed "s/\([aeiouy]\+\)\\1/\\1h/g" "$f" -i
done
