#!/bin/bash

 if [ -z "$1" ]; then 
     echo usage: $0 regexp
     exit
 fi
grep -P -e "$1" freqdist_summ_ALL_16_iBa | sort -k 2 -n
