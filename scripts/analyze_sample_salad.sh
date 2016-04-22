#!/bin/bash

cp sampleSalad.txt s.txt
sed "s/ae/ä/g" s.txt -i
sed "s/oe/ö/g" s.txt -i
sed "s/$/\t1/" s1.txt -i
sed "s/ //g" -i s1.txt
hanalyze harmsummary s1.txt -i all
