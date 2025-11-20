#!/bin/bash
# USAGE:
#
# ./docker-run.sh $BINARY 
#
# Here $BINARY is the name of a binary.
# Example:
#  ./docker-run.sh /usr/bin/ssh .text .plt 


# Build the Docker container from the Dockerfile in the current directory
docker build -t xed .

# Run the Docker container
# If current arch is not x86_64, then try to do emulation (works on Apple M1 chips)
unameOut="$(uname -m)"
case "${unameOut}" in
    arm64*)     docker run --rm -v $1:/data/input --platform linux/amd64 xed /data/input "${@:2}";;
    *)          docker run --rm -v $1:/data/input                                        "${@:2}";;
esac
