FROM haskell:8.10

# install Graphviz (dot)
RUN apt-get -qq update; \
    apt-get install -qqy \
        graphviz

# install capstone 4.0.1
RUN wget https://github.com/aquynh/capstone/archive/4.0.1.zip
RUN unzip 4.0.1.zip
WORKDIR capstone-4.0.1
RUN ./make.sh
RUN ./make.sh install
RUN cabal update


# unzip the PA
WORKDIR /
RUN mkdir artifacts
COPY ballpark.zip ./ballpark.zip
RUN unzip ballpark.zip
WORKDIR /ballpark/


# build PA
RUN cabal build --dependencies-only
RUN cabal build foxdec.cabal

# run PA on the examples
COPY run_PA.sh ./
RUN chmod +x run_PA.sh

ENTRYPOINT ["/ballpark/run_PA.sh"]
