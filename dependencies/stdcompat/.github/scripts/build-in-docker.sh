set -e
ocaml_compiler="$1"
builder="$2"
opam switch "$ocaml_compiler"
eval $(opam env)
sh -exc ".github/scripts/build-\"$builder\".sh"
