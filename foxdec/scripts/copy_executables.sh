#!/bin/bash

# Source directory from first argument, default to current directory if not provided
SRC_DIR="${1:-.}"

# Directory where files will be copied
DEST_DIR=".."

# Find all executable files in source directory
find "$SRC_DIR" -maxdepth 1 -type f -executable ! -name "dcgen" ! -name "libstdbuf.so" | while read -r filepath; do
	filename=$(basename "$filepath")
	echo "Copying $filename to $DEST_DIR/"
	cp -f "$filepath" "$DEST_DIR/$filename"
done
