## =============================================================================
## == BUILD XED ================================================================
## =============================================================================

FROM ubuntu:24.04 AS build-xed
ARG DEBIAN_FRONTEND=noninteractive

WORKDIR /xed
COPY ci/Makefile .
COPY foxdec/xed .

RUN apt-get update && apt-get install -y \
    git \
    python3 python3-pip \
    gcc make \
    libelf-dev \
 && rm -rf /var/lib/apt/lists/*

RUN git clone https://github.com/intelxed/xed.git    xed    && cd xed    && git checkout dc6bdbe
RUN git clone https://github.com/intelxed/mbuild.git mbuild && cd mbuild && git checkout 7c4497f

RUN make xed-deps

COPY foxdec/xed/disasm.c .
RUN make xed

## =============================================================================
## == BUILD FOXDEC =============================================================
## =============================================================================

FROM haskell:9.12.2 AS build-foxdec

WORKDIR /foxdec
COPY ci/Makefile .

COPY foxdec/foxdec.cabal .
RUN make foxdec-deps

COPY --from=build-xed /xed/xed/libxed.so .

COPY foxdec .
RUN make foxdec

## =============================================================================
## == RUNTIME ==================================================================
## =============================================================================

FROM ubuntu:24.04 AS runtime
ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
 && apt-get install -y libelf1 python3 python3-pip python3-pyelftools \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /src
COPY ci/Makefile .
COPY ci     /src/ci
COPY foxdec /src/foxdec

COPY --from=build-foxdec /foxdec/bin/ /staging/foxdec/bin/
COPY --from=build-xed    /xed/disasm/ /staging/xed/disasm/

RUN make install

WORKDIR /workspace
RUN rm -rf /src /staging

