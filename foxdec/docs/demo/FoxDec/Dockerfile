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

# clone the git (branch v0.1)
WORKDIR /
ADD repo-key /
RUN \
  chmod 600 /repo-key && \  
  echo "IdentityFile /repo-key" >> /etc/ssh/ssh_config && \  
  echo "StrictHostKeyChecking no" >> /etc/ssh/ssh_config && \ 
  git clone --depth 1 --branch v0.1 git@github.com:ssrg-vt/FoxDec.git

# build FoxDec
WORKDIR /FoxDec/foxdec/
RUN cabal update
RUN cabal build foxdec.cabal

# run FoxDec on the grader example
RUN cabal run foxdec-exe -- ./config/config_verbose.dhall examples/grader grader

# set entry point to copy and start the demo script
COPY demo.sh ./demo.sh
RUN ["chmod", "+x", "./demo.sh"]
ENTRYPOINT ["./demo.sh"]
