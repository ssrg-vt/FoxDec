#!/bin/bash
# Usage:
#
#   ./run_foxdec $DIR $FOXDEC_OPTIONS
#
# Example:
#
#   ./run_foxdec.sh ./examples/wc_small/ -n wc -i BINARY --Gmetrics
#

# Build the Docker container from the Dockerfile in the current directory
cd foxdec
docker build -t foxdec .

# Run the Docker container
used_dir="$1"
shift
docker run --rm -ti --volume="$(pwd)/../$used_dir/:/binary" foxdec -c ./config/config.dhall -d /binary "$@"
