#!/bin/bash

# Directory to search; use current directory if not specified
DIR="${1:-.}"

# Find executable files and run the command
find "$DIR" -maxdepth 1 -type f -executable ! -name "dcgen" ! -name "du-tests" | while read -r filepath; do
    filename=$(basename "$filepath")
    echo "Processing $filename"
    stack exec foxdec-exe -- -c ./config/config.dhall -v -i BINARY -d "$1" -n "$filename" --GNASM
done
