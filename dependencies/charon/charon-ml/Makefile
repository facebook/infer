# Build, after formatting the code
.PHONY: build
build: format build-dev

# Build, without formatting the code
.PHONY: build-dev
build-dev:
	cd .. && dune build && dune build @doc

# Generate documentation
.PHONY: doc
doc: 
	cd .. && dune build @doc

# The tests.
# You can use the environment variable "CHARON_LOG" to activate the log.
# For instance: `CHARON_LOG=1 make tests`.
# WARNING: This requires the llbc-building tests to have been run. Prefer using
# `make test` in the root directory.
.PHONY: tests
tests:
	cd .. && dune test

# Reformat the code
.PHONY: format
format:
	@# `dune fmt` returns false if it had to change code; we don't care about that.
	dune fmt || true

# Adapted from https://gitlab.inria.fr/fpottier/visitors/blob/master/Makefile.preprocess
PPX := `ocamlfind query ppx_deriving`/ppx_deriving `ocamlfind query ppx_deriving`/show/ppx_deriving_show.cma `ocamlfind query ppx_deriving`/ord/ppx_deriving_ord.cma `ocamlfind query visitors`/ppx/ppx_deriving_visitors.cma

%.processed.ml: %.ml
	@ echo Preprocessing $<...
	@ ocamlfind ppx_tools/rewriter -ppx '$(PPX)' $< \
		| sed -e '/VISITORS.BEGIN/,/VISITORS.END/!d;//d'\
		| ocamlformat --name="$<" - > $@
