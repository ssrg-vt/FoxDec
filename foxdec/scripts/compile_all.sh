#!/bin/bash

# Check if a directory is provided as an argument
if [ $# -ne 1 ]; then
    echo "Usage: $0 <directory>"
    exit 1
fi

# Directory containing the .o files
TARGET_DIR="$1"

# Path to __gmon_start__.o
GMON_OBJ="$TARGET_DIR/__gmon_start__.o"

# Check if __gmon_start__.o exists
if [ ! -f "$GMON_OBJ" ]; then
    gcc -c "$TARGET_DIR/__gmon_start_.c" -o "$GMON_OBJ"
    if [ $? -ne 0 ]; then
        echo "Failed to compile __gmon_start_.c"
        exit 1
    fi
    echo "Compiled __gmon_start_.c to $GMON_OBJ"
fi

# Loop through all .o files in the directory excluding __gmon_start__.o
for obj_file in "$TARGET_DIR"/*.o; do
    # Skip if the file is __gmon_start__.o
    if [ "$obj_file" == "$GMON_OBJ" ]; then
        continue
    fi

    # Get the base name without the .o extension
    base_name="${obj_file%.o}"
    base_name="${base_name##*/}" # Extract the file name without the path

    # Compile using gcc with the specified options
    echo "Compiling $obj_file -> $base_name"
    gcc -g -m64 -nostartfiles -fgnu-tm -o "$TARGET_DIR/$base_name" "$GMON_OBJ" "$obj_file" -lacl -lz -lselinux -lcrypto -lattr -lgmp

    # Check for success
    if [ $? -ne 0 ]; then
        echo "Failed to compile $obj_file"
    fi
done
