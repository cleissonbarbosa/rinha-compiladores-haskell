FROM haskell:9.2.8-slim-buster

WORKDIR /opt/rinha-compiladores

# Copy over the source files and build
COPY . /opt/rinha-compiladores/
RUN cabal update && cabal install --only-dependencies -j4
RUN cabal build

RUN echo "#!/bin/sh" >> /opt/run.sh
RUN echo "cabal -v0 new-run rinha-compiladores /var/rinha/source.rinha.json" >> /opt/run.sh

ENTRYPOINT ["/bin/bash", "/opt/run.sh"]