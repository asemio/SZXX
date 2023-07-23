# This Dockerfile is used for fuzzing tests

FROM asemio/mountain-caravan:2.1.0
WORKDIR /app
RUN sudo apk update \
  && sudo apk upgrade \
  && sudo apk add --no-cache perl cmake npm xz afl++

COPY SZXX.opam .

ENV DUNE_PROFILE release

RUN opam update \
  && OPAMYES=1 opam install . --deps-only -t

COPY src src
COPY util util
COPY dune-project dune-project

RUN sudo chown -R opam /app

RUN echo '=== Building ===' \
  && opam exec -- dune build util/cli.exe; \
  cp _build/default/util/cli.exe cli.exe; \
  chmod 755 cli.exe
