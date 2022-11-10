#!/bin/bash



# TOOL DEPENDENCIES:
# readelf, tested with GNU readelf (GNU Binutils) 2.35.1

if [[ $# -ne 2 ]]; then
  echo "Usage: "
  echo "./get_elf_entry.sh BINARY NAME"
  echo ""
  echo "  BINARY == binary under investigation"
  echo "  NAME   == name of file to be generated"
  echo ""
  echo "Example usage:"
  echo "./get_elf_entry.sh /usr/bin/du du"
  exit 1
fi


# OBJDUMP
# First, just run objdump for debugging purposes
objdump -M intel --no-show-raw-insn --disassembler-options=hex -d $1 > $2.objdump
echo "Created $2.objdump"

# SYMBOLS
# Second, create a list of all external symbols
# Obtain symbol list, grep all lines that start with 0, remove double spaces, take columns 1 and 5, and prepend "0x"
readelf --wide --relocs $1 | grep '^[0]' | tr -s ' ' | cut -d ' ' -f1,5 | awk '{print "0x" $0}' > $2.symbols
nm -D $1 | grep ^[0] | grep " D " | cut -d ' ' -f1,3 | awk '{print "0x" $0}' >> $2.symbols
echo "Created $2.symbols"

#ENTRY
# remove .entry file, run readelf to read header, search for Entry and takes last word of line
rm -f $2.entry;
if [ "${1: -3}" == ".so" ]; then
  nm  --defined-only $1 | grep " T " | awk '{print "0x" $0}' | cut -d ' ' -f1 > $2.entry
else
  readelf -h $1 | grep Entry | awk 'NF>1{print $NF}' > $2.entry
fi
echo "Created $2.entry"

