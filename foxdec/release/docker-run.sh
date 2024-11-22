#!/bin/bash

# Build the Docker container from the Dockerfile in the current directory
docker build -t ballpark .

# Make a directory artifacts that is to be populated by the PA
mkdir -p artifacts

# Run the Docker container
docker run --rm -ti --volume="$(pwd)/artifacts/:/artifacts" ballpark


