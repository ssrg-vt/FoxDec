## =============================================================================
## == BUILD FOXDEC =============================================================
## =============================================================================

FROM haskell:9.12.2 AS build-foxdec

WORKDIR /foxdec

COPY foxdec/foxdec.cabal .
RUN cabal update && cabal v2-build --only-dependencies

COPY foxdec .
RUN cabal v2-build
RUN cabal v2-install --installdir=/foxdec/bin --overwrite-policy=always

## =============================================================================
## == BUILD XED ================================================================
## =============================================================================

FROM ubuntu:24.04 AS build-xed
ARG DEBIAN_FRONTEND=noninteractive

WORKDIR /xed
COPY foxdec/xed .

RUN apt-get update && apt-get install -y \
    git \
    python3 python3-pip \
    gcc make \
    libelf-dev \
 && rm -rf /var/lib/apt/lists/*

RUN git clone https://github.com/intelxed/xed.git    xed    && cd xed    && git checkout dc6bdbe
RUN git clone https://github.com/intelxed/mbuild.git mbuild && cd mbuild && git checkout 7c4497f

WORKDIR /xed/xed/
RUN ./mfile.py install

WORKDIR /xed/disasm/
COPY foxdec/xed/disasm.c .
RUN gcc -o disasm disasm.c -I /xed/xed/include/public/xed/ -I /xed/xed/obj/wkit/include/xed/ -L /xed/xed/obj/wkit/lib/ -lxed -lelf

## =============================================================================
## == RUNTIME ==================================================================
## =============================================================================

FROM ubuntu:24.04 AS runtime
ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
 && apt-get install -y libelf1 python3 python3-pip python3-pyelftools \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=build-foxdec /foxdec/bin/foxdec-exe /app/foxdec
COPY --from=build-xed    /xed/disasm/disasm     /app/disasm
COPY foxdec/ehframe_reader/ellf_cfi_patch.py    /app/ellf_cfi_patch

RUN chmod +x /app/ellf_cfi_patch
ENV PATH="/app:${PATH}"

