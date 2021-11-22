#!/bin/bash



# TOOL DEPENDENCIES:
# otool
# nm

if [[ $# -ne 2 ]]; then
  echo "Usage: "
  echo "./dump_macho.sh BINARY NAME"
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
objdump -no-show-raw-insn -disassemble -x86-asm-syntax=intel -print-imm-hex $1 > $2.objdump
echo "Created $2.objdump"

# SYMBOLS
# Second, create a list of all external symbols
otool -I -v $1 | grep '^0x' | tr -s ' ' | cut -d ' ' -f1,3 > $2.symbols
echo "Created $2.symbols"


# DUMP/ENTRY/SECTIONS
# Use otool to get an overview of all load-commands of the macho-file
# For each section in a __TEXT segment, dump the bytes.
# Moroever, dump the bytes of (__DATA,__const).

# If an entryoff line is found, print it in hex to a .entry file

# Store information on all sections (addresses and sizes) in .sections file.
# In the .sections file, the sections that may be modified by **external** functions are annotated with preceding |
# For MachO that is (__DATA,__common), for ELF it is is the SHN_COMMON sections.
rm -f $2.dump;
rm -f $2.entry;
rm -f $2.sections;

# We are expecting the following lines in order, line by line.
declare -a expected_lines=("Section" "sectname" "segname" "addr" "size")

num_of_expected_lines=${#expected_lines[@]};
expecting=0;

current_sect_name="";
current_seg_name="";
current_addr="";
current_size="";

# Execute the otool command and read line by line
while IFS= read -r line
do
   #trim spaces (leading and end)
   line=`echo $line | sed -e 's/^[[:space:]]*//g' -e 's/[[:space:]]*\$//g'`
   if [[ $line == ${expected_lines[$expecting]}* ]] ;
   then
      expecting=$((expecting+1));
   else
      expecting=0;
   fi

   if [ "$expecting" -eq 2 ];
   then #reading sectname, take last word of line
      current_sect_name=`echo $line | awk '{ print $NF }'`
   fi
   if [ "$expecting" -eq 3 ];
   then #reading segname, take last word of line
      current_seg_name=`echo $line | awk '{ print $NF }'`
   fi
   if [ "$expecting" -eq 4 ];
   then #reading addr, take second word of line
      current_addr=`echo $line | tr -s ' ' | cut -d ' ' -f2`
   fi
   if [ "$expecting" -eq 5 ];
   then #reading size, take second word of line
      current_size=`echo $line | tr -s ' ' | cut -d ' ' -f2`

      echo "($current_seg_name,$current_sect_name)" >> $2.sections
      echo "  addr = $current_addr" >> $2.sections
      echo "  size = $current_size" >> $2.sections
      if [[ $current_seg_name == "__TEXT" ]]; then
        # -n +2 : start at line 2 of the file, skipping the first line.
        otool -s $current_seg_name $current_sect_name $1 | tail -n +2  >> $2.dump
      fi
      if [[ $current_seg_name == "__DATA" ]] && [[ $current_sect_name == "__const" ]] ; then
        # -n +2 : start at line 2 of the file, skipping the first line.
        otool -s $current_seg_name $current_sect_name $1 | tail -n +2  >> $2.dump
      fi
      if [[ $current_seg_name == "__DATA_CONST" ]] && [[ $current_sect_name == "__const" ]] ; then
        # -n +2 : start at line 2 of the file, skipping the first line.
        otool -s $current_seg_name $current_sect_name $1 | tail -n +2  >> $2.dump
      fi
      expecting=0;
   fi

   if [[ $line == "entryoff"* ]]  ; then
      # found entry, print in hex to file
      entry=`echo $line | awk '{ print $NF }'`
      printf '0x%x' $((0x100000000 + $entry)) >> $2.entry
   fi

done < <(otool -l ${1})

if [[ -f "$2.dump" ]]; then
   echo "Created $2.dump"
fi
if [[ -f "$2.sections" ]]; then
   echo "Created $2.sections"
fi
if [[ -f "$2.entry" ]]; then
   echo "Created $2.entry"
else
   # No entry found in the load commands, so it is a library (.dylib)
   nm  --defined-only $1 | grep " T " | awk '{print "0x" $0}' | cut -d ' ' -f1 > $2.entry
   echo "No load-command found that provides entry, so treating binary as library and all defined symbols in text sections as entry points."
   echo "Created $2.entry"
fi


