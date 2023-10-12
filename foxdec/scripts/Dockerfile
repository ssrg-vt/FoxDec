FROM --platform=linux/amd64 ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update -y && apt-get install build-essential gdb nasm vim less binutils strace -y



RUN apt-get install git autoconf bison autopoint gettext gperf texinfo rsync wget -y
RUN mkdir /coreutils
RUN mkdir /coreutils/local
RUN cd /coreutils && git clone --depth 1 https://github.com/coreutils/coreutils.git
RUN cd /coreutils/coreutils && ./bootstrap
RUN cd /coreutils/coreutils && ./configure FORCE_UNSAFE_CONFIGURE=1
RUN cd /coreutils/coreutils && make 

RUN apt-get install libgss-dev libssl-dev libselinux-dev libz-dev openssh-client -y

RUN echo '#!/bin/bash\necho "FoxDec NASM: compiling .asm to .o file"\necho "nasm -felf64 -o $BINARY.o $BINARY.asm"\nnasm -felf64 -g -F dwarf -o $BINARY.o $BINARY.asm' > /usr/bin/foxdec_nasm && \
    chmod +x /usr/bin/foxdec_nasm

RUN echo '#!/bin/bash\necho "FoxDec GCC: compiling .o to executable"\necho "gcc -g -m64 -nostartfiles -fgnu-tm -o a.out $BINARY.o __gmon_start__.o"\ngcc -c __gmon_start__.c -o __gmon_start__.o && gcc -g -m64 -nostartfiles -fgnu-tm -o a.out $BINARY.o __gmon_start__.o' > /usr/bin/foxdec_gcc && \
    chmod +x /usr/bin/foxdec_gcc

RUN echo '#!/bin/bash\necho "FoxDec OBJDUMP: run objdump on executable"\necho "objdump -M intel --no-show-raw-insn --disassembler-options=hex -d $BINARY > $BINARY.objdump"\nobjdump -M intel --no-show-raw-insn --disassembler-options=hex -d $BINARY > $BINARY.objdump' > /usr/bin/foxdec_objdump && \
    chmod +x /usr/bin/foxdec_objdump

RUN echo '#!/bin/bash\necho "FoxDec ENTRY: create .entry file for executable"\nrm -f $BINARY.entry;\nif [ "${1: -3}" == ".so" ]; then\n  nm  --defined-only $BINARY | grep " T " | awk '{print "0x" $0}' | cut -d ' ' -f1 > $BINARY.entry\nelse\n  readelf -h $BINARY | grep Entry | awk '"'"'NF>1{print $NF}\'"'"' > $BINARY.entry\nfi\necho "Created $BINARY.entry"\n' > /usr/bin/foxdec_entry && \
    chmod +x /usr/bin/foxdec_entry



WORKDIR /binary/
ENTRYPOINT ["bash"]
