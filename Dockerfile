FROM haskell:9.2.8-slim-buster

WORKDIR /opt/rinha-compiladores

# Copy over the source files and build
COPY . /opt/rinha-compiladores/
RUN cabal update && cabal install --only-dependencies -j4
RUN cabal build

RUN echo "#!/bin/sh" >> /opt/run.sh
RUN echo "cabal run rinha-compiladores ./examples/fib.json" >> /opt/run.sh
RUN echo "cabal run rinha-compiladores ./examples/comb.json" >> /opt/run.sh
RUN echo "cabal run rinha-compiladores ./examples/sum.json" >> /opt/run.sh
RUN echo "cabal run rinha-compiladores ./examples/print.json" >> /opt/run.sh

ENTRYPOINT ["/bin/bash", "/opt/run.sh"]