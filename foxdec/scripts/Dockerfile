FROM --platform=linux/amd64 ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update -y && apt-get install build-essential gdb nasm vim less binutils strace -y

WORKDIR /binary/
ENTRYPOINT ["bash"]
