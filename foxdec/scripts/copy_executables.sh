#!/bin/bash

# Directory where files will be copied
DEST_DIR=".."

# Find all executable files in current directory
find . -maxdepth 1 -type f -executable ! -name "dcgen" ! -name "libstdbuf.so" | while read -r filepath; do
    filename=$(basename "$filepath")
    echo "Copying $filename to $DEST_DIR/"
    cp -f "$filepath" "$DEST_DIR/$filename"
done

