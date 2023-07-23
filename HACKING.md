## Setup

```sh
opam switch create . ocaml-variants.5.0.0+options --deps-only -t
```

## Build

#### tests

```sh
dune runtest -w
```

#### test cli

```sh
dune build util/cli.exe -w
```

## Fuzz

Add `-afl-instrument` to `util/dune` `ocamlopt_flags`

```sh
docker build . -t szxx:latest

docker run -it --rm -v "$(pwd)/test/files:/app/files" szxx:latest
```
Then in the container:
```sh
mkdir -p fuzz/input
mkdir -p fuzz/output
cp files/formatting.xlsx fuzz/input/
cd fuzz
/usr/bin/afl-fuzz -i input -o output -- ../cli.exe count @@
```
