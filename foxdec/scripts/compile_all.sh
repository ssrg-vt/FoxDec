#!/bin/bash

# Path to __gmon_start__.o
GMON_OBJ="__gmon_start__.o"

# Check if __gmon_start__.o exists
if [ ! -f "$GMON_OBJ" ]; then
    echo "Error: $GMON_OBJ not found in the current directory."
    exit 1
fi

# Loop through all .o files excluding __gmon_start__.o
for obj_file in *.o; do
    # Skip if the file is __gmon_start__.o
    if [ "$obj_file" == "$GMON_OBJ" ]; then
        continue
    fi

    # Get the base name without the .o extension
    base_name="${obj_file%.o}"

    # Compile using gcc with the specified options
    echo "Compiling $obj_file -> $base_name"
    gcc -g -m64 -nostartfiles -fgnu-tm -o "$base_name" "$GMON_OBJ" "$obj_file" -lacl -lz -lselinux -lcrypto -lattr -lgmp

    # Check for success
    if [ $? -ne 0 ]; then
        echo "Failed to compile $obj_file"
    fi
done

