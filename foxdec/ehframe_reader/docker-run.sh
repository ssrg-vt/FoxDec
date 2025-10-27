#!/bin/bash
# USAGE:
#
# ./docker-run.sh $BINARY
#
# Here $BINARY is the name of a binary  

if [ -z "$1" ]; then
  echo "Usage: $0 <path>"
  exit 1
fi

# Extract directory and filename
dir=$(dirname "$1")
file=$(basename "$1")


# Convert directory to absolute path
if [[ "$dir" != /* ]]; then
  dir="$(realpath "$dir")"
fi



# Build the Docker container from the Dockerfile in the current directory
docker build -t ehframe .

# Run the Docker container
# If current arch is not x86_64, then try to do emulation (works on Apple M1 chips)
unameOut="$(uname -m)"
case "${unameOut}" in
    arm64*)     docker run --rm -v "$dir"/:/data/input --platform linux/amd64 ehframe generate -o /data/input/"$file".cfi.txt /data/input/"$file";;
    *)          docker run --rm -v "$dir"/:/data/input                        ehframe generate -o /data/input/"$file".cfi.txt /data/input/"$file";;
esac
