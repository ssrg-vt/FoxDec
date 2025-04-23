#!/bin/bash

# Assemble all .asm files in the current directory
for asm_file in *.asm; do
    # Skip if no .asm files are found
    [ -e "$asm_file" ] || continue

    # Get the base filename without extension
    base_name="${asm_file%.asm}"

    # Assemble the file
    echo "Assembling $asm_file -> ${base_name}.o"
    as --noexecstack -o "${base_name}.o" "$asm_file"

    # Check if the command succeeded
    if [ $? -ne 0 ]; then
        echo "Failed to assemble $asm_file"
    fi
done

