#!/bin/bash

# Check if a directory is provided as an argument
if [ -z "$1" ]; then
    echo "Usage: $0 <directory>"
    exit 1
fi

# Get the directory from the argument
directory="$1"

# Check if the provided argument is a valid directory
if [ ! -d "$directory" ]; then
    echo "Error: $directory is not a valid directory"
    exit 1
fi

# Assemble all .asm files in the specified directory
for asm_file in "$directory"/*.asm; do
    # Skip if no .asm files are found
    [ -e "$asm_file" ] || continue

    # Get the base filename without extension
    base_name="${asm_file%.asm}"

    # Assemble the file
    echo "Assembling $asm_file -> ${base_name}.o"
    nasm -felf64 -g -F dwarf -o "${base_name}.o" "$asm_file"

    # Check if the command succeeded
    if [ $? -ne 0 ]; then
        echo "Failed to assemble $asm_file"
    fi
done
