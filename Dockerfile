FROM haskell:9.10.1
RUN ln -s $(which ghc) /usr/bin/ghc-9.10.1
WORKDIR /usr/aoc-2025

COPY aoc2025.cabal ./
COPY cabal.project ./
RUN cabal update
RUN cabal build --only-dependencies

COPY . .
RUN cabal build

ENTRYPOINT [ "./day.sh" ]
