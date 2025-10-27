FROM --platform=linux/amd64 ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -y

RUN apt-get install libelf-dev -y
RUN apt-get install git -y
RUN apt-get install python3 -y
RUN apt-get install gcc -y
RUN apt-get update -y
RUN apt-get install python3-pip -y

RUN pip install pyelftools

RUN mkdir /app
RUN mkdir /data

COPY *.py /app
RUN chmod +x /app/*.py


ENTRYPOINT ["python3", "/app/ellf_cfi_patch.py"]
