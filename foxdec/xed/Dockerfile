FROM --platform=linux/amd64 ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y

RUN apt-get install build-essential gdb less strace binutils cmake curl -y
RUN apt-get install libelf-dev -y
RUN apt-get install git -y
RUN apt-get install python3 -y

RUN mkdir /xed
WORKDIR /xed/

RUN git clone https://github.com/intelxed/xed.git xed
WORKDIR /xed/xed
RUN git checkout dc6bdbe
WORKDIR /xed
RUN git clone https://github.com/intelxed/mbuild.git mbuild
WORKDIR /xed/mbuild
RUN git checkout 7c4497f
WORKDIR /xed/xed/
RUN ./mfile.py examples install


RUN mkdir /xed/disasm/
WORKDIR /xed/disasm/
COPY ./disasm.c ./disasm.c
RUN gcc -o disasm disasm.c -I /xed/xed/include/public/xed/ -I /xed/xed/obj/wkit/include/xed/ -L /xed/xed/obj/wkit/lib/ -lxed -lelf

ENTRYPOINT ["./disasm"]
