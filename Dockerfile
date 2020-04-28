FROM haskell:8.8.3 AS builder
COPY . /app
WORKDIR /app
RUN cabal update
RUN cabal v2-build

FROM debian:buster-slim
WORKDIR /app
COPY --from=builder /app/dist-newstyle/build/x86_64-linux/ghc-8.8.3/the-round-table-0.1.0.0/x/the-round-table/build/the-round-table/the-round-table .
EXPOSE 8080
CMD ["./the-round-table"]