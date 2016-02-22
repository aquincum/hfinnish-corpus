#!/bin/bash

for fn in filtered3_all_quant_*
do
  hanalyze harmsummary -d $fn > harmlex_quant_$fn
  rename "s/harmlex_quant_filtered3_all_quant_(.*)/harmlex_quant_\1.txt/" *
done
