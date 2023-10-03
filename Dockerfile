FROM haskell:9.2.8-slim-buster

WORKDIR /opt/rinha-compiladores

# Copy over the source files and build
COPY . /opt/rinha-compiladores/
RUN cabal update && cabal install --only-dependencies -j4
RUN cabal build
ENV PROJECT_VERSION=0.1
ENV PROJECT_NAME=rinha-compiladores

RUN echo "#!/bin/sh" >> /opt/rinha-compiladores/run.sh
RUN echo "./dist-newstyle/build/x86_64-linux/ghc-9.2.8/$PROJECT_NAME-$PROJECT_VERSION/x/$PROJECT_NAME/build/$PROJECT_NAME/$PROJECT_NAME /var/rinha/source.rinha.json" >> /opt/rinha-compiladores/run.sh

ENTRYPOINT ["/bin/bash", "/opt/rinha-compiladores/run.sh"]