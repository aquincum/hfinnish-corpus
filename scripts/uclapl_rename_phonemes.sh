#!/bin/bash

file=$1
sed s/ä/ae/g $file -i.bak1 
sed s/ö/oe/g $file -i.bak2
