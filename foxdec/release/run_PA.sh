#!/bin/bash

dirs=( spec/401.bzip2 spec/429.mcf spec/456.hmmer spec/458.sjeng spec/462.libquantum spec/464.h264ref spec/470.lbm spec/milc du gzip host sha512sum sort sqlite3 ssh tar vi wc wc_small wget2 xxd zip )
names=( bzip2 mcf hmmer sjeng libquantum h264ref lbm milc du gzip host sha512sum sort sqlite3 ssh tar vim wc wc libwget.so xxd zip )

cd /ballpark/
for idx in "${!dirs[@]}"; do
	cabal run foxdec-exe -- -c ./config/config.dhall -v -i BINARY --Gmetrics -d ./examples/${dirs[$idx]}/ -n ${names[$idx]}
done
cp -r examples/* /artifacts/
