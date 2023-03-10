FROM --platform=linux/amd64 ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update -y && apt-get install build-essential gdb nasm vim less binutils strace -y

RUN echo '#!/bin/bash\necho "FoxDec NASM: compiling .asm to .o file"\necho "nasm -felf64 -o $BINARY.o $BINARY.asm"\nnasm -felf64 -g -F dwarf -o $BINARY.o $BINARY.asm' > /usr/bin/foxdec_nasm && \
    chmod +x /usr/bin/foxdec_nasm

RUN echo '#!/bin/bash\necho "FoxDec GCC: compiling .o to executable"\necho "gcc -g -m64 -nostartfiles -fgnu-tm -o a.out $BINARY.o __gmon_start__.o"\ngcc -c __gmon_start__.c -o __gmon_start__.o && gcc -g -m64 -nostartfiles -fgnu-tm -o a.out $BINARY.o __gmon_start__.o' > /usr/bin/foxdec_gcc && \
    chmod +x /usr/bin/foxdec_gcc

RUN echo '#!/bin/bash\necho "FoxDec OBJDUMP: run objdump on executable"\necho "objdump -M intel --no-show-raw-insn --disassembler-options=hex -d $BINARY > $BINARY.objdump"\nobjdump -M intel --no-show-raw-insn --disassembler-options=hex -d $BINARY > $BINARY.objdump' > /usr/bin/foxdec_objdump && \
    chmod +x /usr/bin/foxdec_objdump

WORKDIR /binary/
ENTRYPOINT ["bash"]
