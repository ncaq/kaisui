FROM mirror.gcr.io/library/rust:1.87

WORKDIR /app

COPY src/ /app/src/
COPY Cargo.lock Cargo.toml ./

RUN --mount=type=cache,target=/root/.cargo/ \
  cargo build --bin server

CMD ["cargo", "run", "--bin", "server"]
