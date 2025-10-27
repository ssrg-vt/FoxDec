#!/bin/bash
# Usage:
#
#   run_xed $BINARY
#
cd xed
./docker-run.sh "$(pwd)/../$1" > "$(pwd)/../$1.xed"
echo Created file "$1.xed"
