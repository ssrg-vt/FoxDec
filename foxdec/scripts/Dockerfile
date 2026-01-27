FROM --platform=linux/amd64 ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y

RUN apt-get install build-essential gdb nasm less strace binutils cmake curl \
                    libssl-dev libgss-dev libselinux-dev libz-dev openssh-client libgss-dev zlib1g-dev selinux-basics -y 

RUN apt-get install ssh vim coreutils sqlite3 zip tar bind9 xxd -y

#RUN apt-get install ssh=1:8.9p1-3ubuntu0.10 -y
#RUN apt-get install vim=2:8.2.3995-1ubuntu2.19 -y 
#RUN apt-get install coreutils=8.32-4.1ubuntu1.2 -y 
#RUN apt-get install sqlite3=3.37.2-2ubuntu0.3 -y 
#RUN apt-get install zip=3.0-12build2 -y 
#RUN apt-get install tar=1.34+dfsg-1ubuntu0.1.22.04.2 -y 
#RUN apt-get install bind9-host=1:9.18.28-0ubuntu0.22.04.1 -y 
#RUN apt-get install xxd=2:8.2.3995-1ubuntu2.19 -y 


RUN echo '#!/bin/bash\necho "FoxDec GAS: compiling .s to .o file"\necho "gcc -g -m64 -nostartfiles -fgnu-tm -o $BINARY.o -c $BINARY.s"\ngcc -g -m64 -nostartfiles -fgnu-tm -o $BINARY.o -c $BINARY.s' > /usr/bin/foxdec_gas && \
    chmod +x /usr/bin/foxdec_gas

RUN echo '#!/bin/bash\necho "FoxDec NASM: compiling .asm to .o file"\necho "nasm -felf64 -o $BINARY.o $BINARY.asm"\nnasm -felf64 -g -F dwarf -o $BINARY.o $BINARY.asm' > /usr/bin/foxdec_nasm && \
    chmod +x /usr/bin/foxdec_nasm

RUN echo '#!/bin/bash\necho "FoxDec gcc: compiling .o to executable"\necho "gcc -g -m64 -nostartfiles -fgnu-tm -o a.out $BINARY.o __gmon_start__.o"\ngcc -c __gmon_start__.c -o __gmon_start__.o && gcc -g -m64 -nostartfiles -fgnu-tm -o a.out $BINARY.o __gmon_start__.o "$@"' > /usr/bin/foxdec_gcc && \
    chmod +x /usr/bin/foxdec_gcc

RUN echo '#!/bin/bash\necho "FoxDec g++: compiling .o to executable"\necho "g++ -g -m64 -nostartfiles -fgnu-tm -o a.out $BINARY.o __gmon_start__.o"\ngcc -c __gmon_start__.c -o __gmon_start__.o && g++ -g -m64 -nostartfiles -fgnu-tm -o a.out $BINARY.o __gmon_start__.o "$@"' > /usr/bin/foxdec_g++ && \
    chmod +x /usr/bin/foxdec_g++

RUN echo '#!/bin/bash\necho "FoxDec OBJDUMP: run objdump on executable"\necho "objdump -M intel --no-show-raw-insn --disassembler-options=hex -d $BINARY > $BINARY.objdump"\nobjdump -M intel --no-show-raw-insn --disassembler-options=hex -d $BINARY > $BINARY.objdump' > /usr/bin/foxdec_objdump && \
    chmod +x /usr/bin/foxdec_objdump

RUN echo '#!/bin/bash\necho "FoxDec ENTRY: create .entry file for executable"\nrm -f $BINARY.entry;\nif [ "${1: -3}" == ".so" ]; then\n  nm  --defined-only $BINARY | grep " T " | awk '\''{print "0x" $0}'\'' | cut -d '\'' '\'' -f1 > $BINARY.entry\nelse\n  readelf -h $BINARY | grep Entry | awk '"'"'NF>1{print $NF}\'"'"' > $BINARY.entry\nfi\necho "Created $BINARY.entry"\n' > /usr/bin/foxdec_entry && \
    chmod +x /usr/bin/foxdec_entry

RUN echo '#!/bin/bash\ncp "$1" /binary/\n' > /usr/bin/foxdec_cp && \
    chmod +x /usr/bin/foxdec_cp



# RUN apt-get install git autoconf bison autopoint gettext gperf texinfo rsync wget -y
# RUN mkdir /coreutils
# RUN mkdir /coreutils/local
# RUN cd /coreutils && git clone --depth 1 https://github.com/coreutils/coreutils.git
# RUN cd /coreutils/coreutils && ./bootstrap
# RUN cd /coreutils/coreutils && ./configure FORCE_UNSAFE_CONFIGURE=1
# RUN cd /coreutils/coreutils && make 


RUN mkdir /binary
WORKDIR /binary/
ENTRYPOINT ["bash"]
