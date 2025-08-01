#!/bin/bash
# Usage:
#
#   run_xed $BINARY
#
cd xed
./docker-run.sh "$(pwd)/../$1" .text .plt .init .fini .plt.sec .plt.got .iplt > "$(pwd)/../$1.xed"
echo Created file "$1.xed"
