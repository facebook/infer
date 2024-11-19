set -e
opam install --yes dune
dune build --ignore-promoted-rules
dune runtest
