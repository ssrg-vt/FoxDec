#!/bin/bash

# Build the Docker container from the Dockerfile in the current directory
docker build -t nasm .

# Run the Docker container
# If current arch is not x86_64, then try to do emulation (works on Apple M1 chips)
unameOut="$(uname -m)"
case "${unameOut}" in
    arm64*)     docker run --rm -ti --volume="$(pwd)/$1/:/binary" -e BINARY=$2 --platform linux/amd64 nasm;;
    *)          docker run --rm -ti --volume="$(pwd)/$1/:/binary" -e BINARY=$2 nasm;;
esac


