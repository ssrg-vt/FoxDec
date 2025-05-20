#!/bin/bash

# Directory to search; use current directory if not specified
DIR="${1:-.}"

# Find executable files and run the command
find "$DIR" -maxdepth 1 -type f -executable ! -name "dcgen" ! -name "du-tests" ! -name "libstdbuf.so" ! -name "wc" ! -name "cksum"| while read -r filepath; do
    filename=$(basename "$filepath")
    basepath=$(dirname "$filepath")
    echo "Processing $filename"
    mkdir -p "$basepath/nasm"
    stack exec foxdec-exe -- -c ./config/config.dhall -v -i BINARY -d "$1" -n "$filename" --GNASM 2> "$basepath/nasm/$filename.err" 1> "$basepath/nasm/$filename.out"
done
