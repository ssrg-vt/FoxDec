import os
import subprocess
import re
import sys
import string

# This file adds a command that can be executed in GDB.
# This command outputs a trace (a list of all executed internal assembly instructions) to FILE
# To use, start GDB and do the following steps
#
#  source ./gdb.py      ; load this script
#  starti               ; load the binary 
#  trace-asm FILE False ; run the command, assuming that __libc_start_main is a recognizable symbol

# Alternatively, run 
#
#  break main
#  trace-asm FILE True ; run the command, assuming that a breakpoint has been introduced at main

# Based on the API described at:
#  https://sourceware.org/gdb/onlinedocs/gdb/Python-API.html

# Evaluate a size directive to an integer
def eval_sizedir(str):
  if str == "XMMWORD": return 16
  if str == "QWORD": return 8
  if str == "DWORD": return 4
  if str == "WORD": return 2
  if str == "BYTE": return 1
  raise Exception("Cannot parse size directive: " + str)

# Evaluate an address in an instruction operand to an integer, based on the current state (a GDB frame)
# str = the address expression
# full = the full instruction (string), used for error reporting
def eval_address(frame,str,full):
  str = str.strip()
  splits = str.split("+")

  if len(splits) > 1:
    addends = map(lambda a : eval_address(frame,a,full),splits)
    return sum(addends)

  splits = str.split("-")
  if len(splits) == 2:
    return eval_address(frame,splits[0],full) - eval_address(frame,splits[1],full)

  splits = str.split("*")
  if len(splits) == 2:
    return eval_address(frame,splits[0],full) * eval_address(frame,splits[1],full)

  if str.startswith("0x") and all(c in string.hexdigits for c in str[2:]):
    return int(str,16)

  if str.isnumeric and len(str) == 1:
    return int(str)

  return int(frame.read_register(str).cast(gdb.lookup_type('long')))
  raise Exception("Cannot parse mem address: " + str)
   
# Evaluate an integer value only if it doens't "smell" like a pointer
def eval_only_if_not_pointer(str,val):
  if abs(val) <= 100000:
    return str + "==" + hex(val)
  else:
    return str
  
# Parse a memory operand to a string
# "Parsing" here means trying to evaluate the memory operand in the current state
# inferior = the GDB inferior
# frame  = the GDB frame
# do_eval  = must the opernad be evaluated?
# str    = string representation of the operand
# full = the full instruction (string), used for error reporting
def parse_mem(inferior,frame,do_eval,str,si,full):
  if len(str) <= 1 or str[0] != '[' or str[-1] != ']':
    raise Exception("Cannot parse mem operand: " + str + " in " + full)

  if not do_eval:
    return "[addr]"

  str = str[1:]
  str = str[:-1]

  addr = eval_address(frame,str,full)

  try:
    val = inferior.read_memory(addr,si)
    return eval_only_if_not_pointer("[" + str + "]",int.from_bytes(val,"little"))
  except:
    raise Exception("Error reading memory: full = " + full + ", addr = " + hex(addr))
  

# Parse an operand to a string
# "Parsing" here means trying to evaluate the operand in the current state
# inferior = the GDB inferior
# frame    = the GDB frame
# do_eval  = must the operand be evaluated?
# str      = string representation of the operand
# full     = the full instruction (string), used for error reporting
def parse_operand(inferior,frame,do_eval,str,full):
  strs = str.split(":", 1)
  if len(strs) > 1:
    return str

  str = str.strip()
  splits = str.split()
  if str.startswith("0x"):
    return "imm"
  elif len(splits) >= 3 and splits[1] == "PTR":
    return " ".join([splits[0],splits[1],parse_mem(inferior,frame,do_eval," ".join(splits[2:]), eval_sizedir(splits[0]), full)])
  elif len(str) > 0 and str[0] == "[":
    return "addr"
  elif not do_eval:
    return str
  else:
    if str.startswith("xmm"):
      return str
    if str.endswith("b"):
      return str # str = str[:-1] + "d"
    try:
      val = frame.read_register(str)
      return eval_only_if_not_pointer(str,val)
    except:
      raise Exception("Error reading register: full = " + full + ", str = " + str)
 

def ignored_opcodes():
  return ["endbr64", "jmp", "bnd", "nop"]

# Parse an instruction to a string
# "Parsing" here means trying to evaluate the operands of the instructions in the current state
# Returns the prefix, the opcode, and evaluates the first operand (often the destination operand)
def parse_instr(inferior,frame,str):
  strs = str.split(" ", 1)
  prefix = ""
  opcode = "UNKNOWN: " + str
  ops  = [] 

  str_no_prefix = ""
  if len(strs) <= 1:
    opcode = str
    return (prefix,opcode,ops)
  elif strs[0] in ["bnd", "rep", "repz", "notrack"]:
    prefix = strs[0]
    str_no_prefix = strs[1]
  else:
    str_no_prefix = str

  [opcode,remainder] = str_no_prefix.split(" ", 1)
  ops = remainder.split('#',1)[0].split('<',1)[0].split(',')

  if opcode in ignored_opcodes():
    return ("",opcode,[])

  ops1 = []
  for i,x in enumerate(ops):
    ops1.append(parse_operand(inferior,frame,i==0,x,str))

  return (prefix,opcode,ops1)



# a GDB command 'trace-asm'
# in GDB, execute:
#
#    starti
#    trace-asm FILE {True/False}
#
# Writes into FILE all executed assembly instructions + their effects
# If the binary starts with calling __libc_start_main, and that symbol is recognizable as such, provide as second argument True.
# Otherwise, first set a breakpoint to the main function manually, and use False. 

# The command first finds the .text section of the currently loaded binary.
# It assumes that when running "info file" in GDB, the first .text section is the one of interest.
# It obtains the lower-and upperbound of that text section.
# It then runs the binary:
#
#  If provided True it breaks at all function calls. As soon as _libc_start_main is executed, it stops breaking at all function calls
#  and instead breaks at the value currently stored in RDI (i.e., the "main" function).
#  If provided False it breaks at "main" assuming that break was set manually.
#
# From that point on, all instructions with RIP in range between lower- and upperbound are traced. As soon as another instruction is executed,
# a break is set at the current return address and we continue running without tracing until that breakpoint.
class TraceAsm(gdb.Command):
  def __init__(self):
    super().__init__(
      'trace-asm',
      gdb.COMMAND_BREAKPOINTS,
      gdb.COMPLETE_NONE,
      False
    )

  def invoke(self, argument, from_tty):
    argv = gdb.string_to_argv(argument)
    if len(argv) != 2:
      gdb.write('Provide as arguments 1.) file name for log and 2.) \"True\" iff the binary contains __libc_start_main. Otherwise, it is assumed there is breakpoint set at main.\n')
    else:
      argv = gdb.string_to_argv(argument)
      with open(argv[0], 'w') as f:
        binary_name = gdb.current_progspace().filename


        gdb.execute("set pagination off", to_string=False)
        gdb.execute("set logging redirect on", to_string=False)
        gdb.execute("set logging file /dev/null", to_string=False)
        gdb.execute("set verbose off", to_string=False)
        gdb.execute("set complaints 0", to_string=False)
        gdb.execute("set disassembly-flavor intel", to_string=False)

        # Obtain the bounds of the .text section of the binary (internal text section only)
        lbound = -1
        ubound = -1
        for line in gdb.execute("info file", to_string=True).splitlines():
          if lbound == ubound and line[-5:] == ".text":
            lbound = int(line.split("-")[0].strip(), 16)
            ubound = int(line.split("-")[1].split()[0], 16)
        if lbound == ubound:
          raise Exception("Could not find text section in: " + gdb.execute("info file", to_string=True))
          

        if argv[1] == "True":
          gdb.execute("rbreak .")
          started = False
        else:
          started = True
        gdb.execute("c", to_string=False)

        thread = gdb.inferiors()[0].threads()[0]
        while thread.is_valid():
          try:
            frame = gdb.selected_frame()
          except:
            f.write("ERROR ENDING!")
            return
          name = frame.name()
          pc = frame.pc()

          if name != None and "_libc_start_main" in name and not started:
            started = True
            gdb.execute("delete", to_string=False)
            rdi_value = frame.read_register('rdi')
            gdb.execute("break *" + hex(rdi_value), to_string=False)
            gdb.execute('c', to_string=False)
          elif started and lbound <= pc and pc < ubound: #   name in funcs:
            started = True
            instr = frame.architecture().disassemble(pc)[0]['asm']
            gdb.execute("si", to_string=False)
            (prefix,opcode,ops) = parse_instr(gdb.inferiors()[0],gdb.selected_frame(),instr)
            if opcode not in ignored_opcodes():
              f.write(prefix + " " + opcode + " " + ",".join(ops) + "\n")
          elif not started:
            #f.write("Not started, skipping {} {}".format(name, os.linesep))
            gdb.execute("c", to_string=False)
          else:
            rsp_value   = frame.read_register('rsp')
            ret_address = int.from_bytes(gdb.inferiors()[0].read_memory(rsp_value,8),"little")

            #f.write("Skipping {} {} {} to {} {}".format(hex(pc), frame.architecture().disassemble(pc)[0]['asm'], name, hex(ret_address), os.linesep))
            if lbound <= ret_address and ret_address < ubound:
              gdb.execute("break *" + hex(ret_address), to_string=False)
            gdb.execute("c", to_string=False)

TraceAsm()



#         # get relocation for __libc_start_main
#        libc_start_main_address = int("0x" + subprocess.check_output("readelf -r " + binary_name + " | grep libc_start_main", shell=True,input=None).decode('ascii').split()[0],0)

          #f.write("libc = " + hex(real_entry - entry + libc_start_main_address))


        # Retrieve all internal symbols of the binary using nm
        # Instructions within frames of these symbols will be traced.
#        results = subprocess.check_output("nm --defined-only " + binary_name + " | grep \" [tT] \"", shell=True,input=None).decode('ascii')
#        funcs = []
#        for line in results.splitlines():
#          funcs.append(line.split()[2])


