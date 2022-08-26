#!/bin/bash



# TOOL DEPENDENCIES:
# readelf, tested with GNU readelf (GNU Binutils) 2.35.1

if [[ $# -ne 2 ]]; then
  echo "Usage: "
  echo "./dump_elf.sh BINARY NAME"
  echo ""
  echo "  BINARY == binary under investigation"
  echo "  NAME   == name of file to be generated"
  echo ""
  echo "Example usage:"
  echo "./dump_macho.sh /usr/bin/du du"
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



# DUMP/SECTIONS
# Use readelf to get an overview of all sections of the ELF file
# For each relevant section in dump the bytes.

# Store information on all sections (addresses and sizes) in .sections file.
rm -f $2.dump;
rm -f $2.data;
rm -f $2.sections;

current_sect_name="";
current_seg_name="";
current_addr="";
current_size="";

# Execute the otool command and read line by line
while IFS= read -r line
do
   # trim spaces (leading and end)
   # remove double spaces
   # replace "[ " with "["
   line=`echo $line | sed -e 's/^[[:space:]]*//g' -e 's/[[:space:]]*\$//g' | tr -s ' ' | sed -e 's/\[ /\[/g'`

   found_section=false
   found_data_section=false
   found_relevant_section=false
   if [[ $line == *".text "* ]]; then
      current_sect_name=".text"
      found_section=true;
      found_relevant_section=true;
   fi
   if [[ $line == *".init "* ]]; then
      current_sect_name=".init"
      found_section=true;
      found_relevant_section=true;
   fi
   if [[ $line == *".rodata "* ]]; then
      current_sect_name=".rodata"
      found_section=true;
      found_relevant_section=true;
   fi
   if [[ $line == *" .plt "* ]]; then
      current_sect_name=".plt"
      found_section=true;
      found_relevant_section=true;
   fi

   if [[ $line == *".data "* ]]; then
      current_sect_name=".data"
      found_section=true;
      found_data_section=true;
   fi
   if [[ $line == *".bss"* ]]; then
      current_sect_name=".bss"
      found_section=true;
   fi
   if [[ $line == *".got "* ]]; then
      current_sect_name=".got"
      found_section=true;
   fi
   if [[ $line == *".got.plt"* ]]; then
      current_sect_name=".got.plt"
      found_section=true;
   fi
   if [[ $line == *".plt.got"* ]]; then
      current_sect_name=".plt.got"
      found_section=true;
      found_relevant_section=true;
   fi
   if [[ $line == *".plt.sec"* ]]; then
      current_sect_name=".plt.sec"
      found_section=true;
      found_relevant_section=true;
   fi
   if [[ $line == *".init_array"* ]]; then
      current_sect_name=".init_array"
      found_section=true;
      found_relevant_section=true;
   fi
   if [[ $line == *".fini_array"* ]]; then
      current_sect_name=".fini_array"
      found_section=true;
      found_relevant_section=true;
   fi
   if [[ $line == *".data.rel.ro"* ]]; then
      current_sect_name=".data.rel.ro"
      found_section=true;
      found_relevant_section=true;
   fi

   if [ "$found_relevant_section" = true ] ; then
      # dump section, remove empty lines, remove lines with "Hex dump", trim spaces (leading and end), remove double spaces, takes columns 1 to 5
      echo "Contents of $current_sect_name section" >> $2.dump
      readelf --wide --hex-dump=$current_sect_name $1 | grep . | grep -v "Hex dump" | sed -e 's/^[[:space:]]*//g' -e 's/[[:space:]]*\$//g' | tr -s ' ' | cut -d ' ' -f1,2,3,4,5 >> $2.dump
   fi
   if [ "$found_data_section" = true ] ; then
      # dump section, remove empty lines, remove lines with "Hex dump", trim spaces (leading and end), remove double spaces, takes columns 1 to 5
      echo "Contents of $current_sect_name section" >> $2.data
      readelf --wide --hex-dump=$current_sect_name $1 | grep . | grep -v "Hex dump" | sed -e 's/^[[:space:]]*//g' -e 's/[[:space:]]*\$//g' | tr -s ' ' | cut -d ' ' -f1,2,3,4,5 >> $2.data
   fi
   if [ "$found_section" = true ] ; then
      # the address/size is found in columns 4/6 of the line
      current_addr=`echo $line | cut -d ' ' -f4 | awk '{print "0x" $0}'`
      current_size=`echo $line | cut -d ' ' -f6 | awk '{print "0x" $0}'`

      echo "($current_seg_name,$current_sect_name)" >> $2.sections
      echo "  addr = $current_addr" >> $2.sections
      echo "  size = $current_size" >> $2.sections
   fi
done < <(readelf --wide --sections ${1})

if [[ -f "$2.dump" ]]; then
   echo "Created $2.dump"
fi
if [[ -f "$2.data" ]]; then
   echo "Created $2.data"
fi
if [[ -f "$2.sections" ]]; then
   echo "Created $2.sections"
fi

