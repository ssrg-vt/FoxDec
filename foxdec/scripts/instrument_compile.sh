#!/bin/bash

# Usage check
if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <directory>"
  exit 1
fi

TARGET_DIR="$1"

# Verify that the directory exists
if [[ ! -d "$TARGET_DIR" ]]; then
  echo "Error: Directory '$TARGET_DIR' does not exist."
  exit 1
fi

# Set AFL_PATH to default if not already defined
if [[ -z "$AFL_PATH" ]]; then
  AFL_PATH="/home/basavesh/research/RetroWrite-Tutorial/aflplusplus"  # Default path; change if needed
  echo "[*] AFL_PATH not set, using default: $AFL_PATH"
else
  echo "[*] Using AFL_PATH from environment: $AFL_PATH"
fi

# Extra source file and compilation flags
EXTRA_SRC="$TARGET_DIR/__gmon_start__.c"
CFLAGS="-nostartfiles -fgnu-tm"
LDFLAGS="-lacl -lz -lselinux -lcrypto -lattr -lgmp"

# Check for afl-gcc
if [[ ! -x "$AFL_PATH/afl-gcc" ]]; then
  echo "Error: afl-gcc not found in AFL_PATH: $AFL_PATH"
  exit 1
fi

# Compile each .s file in the specified directory
for asm_file in "$TARGET_DIR"/*.s; do
  [[ -e "$asm_file" ]] || continue

  base_name="$(basename "$asm_file" .s)"
  output_bin="$TARGET_DIR/$base_name"

  echo "[*] Compiling $asm_file -> $output_bin"
  AFL_DEBUG=1 AFL_AS_FORCE_INSTRUMENT=1 \
  "$AFL_PATH/afl-gcc" $CFLAGS "$asm_file" "$EXTRA_SRC" -o "$output_bin" $LDFLAGS

  if [[ $? -ne 0 ]]; then
    echo "[!] Compilation failed for $asm_file"
  else
    echo "[+] Successfully compiled $asm_file -> $output_bin"
  fi
done
