FROM mirror.gcr.io/library/haskell:9.10

WORKDIR /app

COPY app/ /app/app/
COPY src/ /app/src/
COPY test/ /app/test/
COPY LICENSE kaisui-distributed-process.cabal ./

RUN --mount=type=cache,target=/root/.local/state/cabal/ \
  cabal update --disable-optimization && \
  cabal install --disable-optimization --install-method=copy

CMD ["server"]
