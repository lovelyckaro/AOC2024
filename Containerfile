FROM docker.io/haskell:9.10.1-slim-bullseye AS build

# upgrade cabal
RUN cabal update
RUN cabal install cabal-install

WORKDIR /opt/aoc
COPY ./aoc2024.cabal ./
# Download and build deps
RUN cabal build --dependencies-only -j
COPY . ./
RUN cabal install --install-method=copy --installdir=./out

FROM docker.io/debian:bullseye-slim
COPY --from=build /opt/aoc/out/aoc /usr/bin/aoc
RUN chmod +x /usr/bin/aoc
ENTRYPOINT [ "/usr/bin/aoc", "--serve" ]
