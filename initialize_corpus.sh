#!/bin/bash

if [ ! -d bigfinnish ]; then
	mkdir bigfinnish
fi
cd bigfinnish
curl http://hlt.sztaki.hu/resources/corp/finnish.tok.gz -o finnish.tok.gz
gunzip finnish.tok.gz
split -l 100000 finnish.tok
create_fds x* 
