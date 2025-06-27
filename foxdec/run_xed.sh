#!/bin/bash
# Usage:
#
#   run_xed $BINARY
#
./xed/docker-run.sh "$(pwd)/$1" .text .plt .init .fini .plt.sec .plt.got > "$(pwd)/$1.xed"
