# vim: nospell
FROM tmcdonell/accelerate-llvm

# Copy over just the cabal and stack file and install dependencies
WORKDIR /opt/rinha-compiladores
COPY ./stack.yaml /opt/rinha-compiladores/stack.yaml
COPY ./rinha-compiladores.cabal /opt/rinha-compiladores/
RUN stack build rinha-compiladores \
  --only-dependencies \
  --flag rinha-compiladores

# Copy over the source files and build
COPY . /opt/rinha-compiladores
RUN stack install --flag rinha-compiladores

CMD ["bash"]
