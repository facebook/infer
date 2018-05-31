# Copyright (c) 2018-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

SHELL=bash

OCAMLDOT?=ocamldot
OCAMLFORMAT?=ocamlformat

.PHONY: default
default: exe

.PHONY: src/version.ml
src/version.ml:
	@tools/gen_version.sh $@

.PHONY: version
version: src/version.ml

jbuild-workspace: jbuild-workspace.in
	sed -e "s|@OPAM_SWITCH[@]|$$(opam switch show)|g" $< > $@

JBUILDS=$(patsubst %.in,%,$(shell find * -name jbuild.in))

.PHONY: jbuilds
jbuilds: $(JBUILDS)

%/jbuild: jbuild-common.in %/jbuild.in
	@cat $+ > $@

.PHONY: setup
setup: src/version.ml jbuild-workspace jbuilds

.PHONY: exe
exe: setup
	jbuilder build src/sledge.exe

.PHONY: bc
bc: setup
	jbuilder build _build/dbg/src/sledge.bc

.PHONY: dbg
dbg: setup
	jbuilder build _build/dbg/src/sledge.exe

.PHONY: opt
opt: setup
	jbuilder build _build/default/src/sledge.exe

SRCS=$(shell \ls src/{,*/}*.ml{,i})

mod_dep.dot: $(SRCS)
	ocamldep.opt $(SRCS) | $(OCAMLDOT) -r Sledge -fullgraph > _build/mod_dep.dot

.PHONY: clean
clean:
	rm -rf _build jbuild-workspace $(JBUILDS)

.PHONY: cleaner
cleaner: clean
	rm -rf src/version.ml

.PHONY: fmt
fmt:
	$(OCAMLFORMAT) -i $(SRCS)
	clang-format -i model/llair_intrinsics.h model/cxxabi.cpp

# print any variable for Makefile debugging
print-%:
	@printf '$*='; printf '$($*)'; printf '\n'
