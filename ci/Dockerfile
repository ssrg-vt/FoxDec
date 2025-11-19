## =============================================================================
## == BUILD XED ================================================================
## =============================================================================

FROM ubuntu:24.04 AS build-xed
ARG DEBIAN_FRONTEND=noninteractive

WORKDIR /xed
COPY ci/Makefile .

RUN apt-get update && apt-get install -y \
    git \
    python3 python3-pip \
    gcc make \
    libelf-dev \
 && rm -rf /var/lib/apt/lists/*

RUN git clone https://github.com/intelxed/xed.git    xed    && cd xed    && git checkout dc6bdbe
RUN git clone https://github.com/intelxed/mbuild.git mbuild && cd mbuild && git checkout 7c4497f

COPY foxdec/xed .

RUN make xed-deps
RUN make xed

## =============================================================================
## == BUILD FOXDEC =============================================================
## =============================================================================

FROM haskell:9.12.2 AS build-foxdec
ENV LD_LIBRARY_PATH=/usr/local/lib

COPY --from=build-xed /xed/libxed.so /usr/local/lib/

WORKDIR /foxdec
COPY ci/Makefile .

COPY foxdec/foxdec.cabal .
RUN make foxdec-deps

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

COPY --from=build-foxdec /foxdec/bin/   /staging/foxdec/bin/
COPY --from=build-xed    /xed/libxed.so /staging/libxed.so

RUN make install

WORKDIR /workspace
RUN rm -rf /src /staging

