#!/bin/bash

for f in cleaned_*
do
  ./onlyfreqn.js 100 $f > $f.100
  hanalyze1 -t uclapl $f.100 2> logs 
  mv Training.txt Training_$f.txt
  echo "" >> Training_$f.txt
  echo "$f done"
done
