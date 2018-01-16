# Copyright (c) 2018 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

# Dead code detection: type `make` in this directory.
#
# OCaml will not detect dead code as soon as it gets exported in a .mli file. But, it will detect
# dead code in inlined modules, even if they have a signature. This suggests the following idea,
# which is basically what happens here:
#
# 1. Put all of the infer source code into a single .ml file with an empty .mli. Each file is put
#   inside its own inline module, with its original signature included too to avoid ambiguities in
#   case of locally opened modules (which may shadow more values than intended otherwise).
# 2. Add preprocessor instructions so that OCaml warnings are shown in the original files.
# 3. Suppress warnings in source code copied from generated files (atdgen, lexers, and parsers).
# 3. Run the OCaml compiler.
# 4. Kill detected dead code; repeat until dry.
#
# Infer is first compiled so that atdgen, ocamllex, and menhir generate the appropriate files. The
# ocamllex and menhir files are generated inside ../_build by jbuilder, and copied here by this
# Makefile.
#
# ocamldepend is used to `cat` the source files in the right order into all_infer_in_one_file.ml.
#
# Beware that this is mostly a terrible hack.

ROOT_DIR = ../../..
include $(ROOT_DIR)/Makefile.config

default: detect_dead_code

ml_src_files_from_mlly:=$(shell find .. -not -path "../*stubs*" -regex '\.\./[a-zA-Z].*\.ml\(l\|y\)')

ml_src_files:=$(shell cd .. && find . -not -path "./*stubs*" -regex '\./[a-zA-Z].*\.ml\(i\)*')

ml_src_files_without_mli:=$(shell cd .. && for i in */*.ml */*/*.ml; do [ -f $${i}i ] || echo $$i; done)

.PHONY: depend
depend:
	cd .. && \
	ocamldep -native \
	  -I IR -I absint -I atd -I backend -I base -I bufferoverrun -I checkers \
	  -I clang -I concurrency -I eradicate -I facebook -I integration -I istd \
	  -I java -I labs -I python -I quandary -I unit -I unit/clang -I deadcode \
	   $(ml_src_files) > deadcode/.depend

%.cmi:
#	deal with the .ml *and* the .mli at the same time: when all the modules are inlined in one
#	file, you need the module value to be defined before you can refer to it, even in
#	signatures. Because of this, the order given by ocamldep is not enough to avoid "Unbound
#	module MyModule" errors in the case of signatures referring to other modules.
	$(QUIET)echo "(* START OF SIGNATURE $*.mli *)" >> all_infer_in_one_file.ml
#	put too many spaces in general but you never know what the contents of the file is;
#	sometimes spaces will be needed
	$(QUIET)echo "  module type " >> all_infer_in_one_file.ml
#	suppress some warnings for generated code
	$(QUIET)if [[ $@ =~ (atd|deadcode)/ ]]; then echo '  [@warning "-27-32-34-35-39"] ' >> all_infer_in_one_file.ml; fi
#	compute module name from file name: capitalize first letter
	$(QUIET)echo $(shell basename $*) | sed -e "s/\b\(.\)/  \u\1/g" >> all_infer_in_one_file.ml
	$(QUIET)echo "  = sig " >> all_infer_in_one_file.ml
#	pre-processor directive to get errors in the original files and not in all_infer_in_one_file.ml
	$(QUIET)echo '# 1 "$*.mli"' >> all_infer_in_one_file.ml
	cat ../$*.mli >> all_infer_in_one_file.ml
	$(QUIET)echo "  end " >> all_infer_in_one_file.ml
	$(QUIET)echo "(* END OF SIGNATURE $*.mli *)" >> all_infer_in_one_file.ml
	$(QUIET)echo >> all_infer_in_one_file.ml
#	lots of duplication from above, sorry
	$(QUIET)echo "(* START OF MODULE $*.ml *)" >> all_infer_in_one_file.ml
	$(QUIET)echo "  module " >> all_infer_in_one_file.ml
	$(QUIET)if [[ $@ =~ (atd|deadcode)/ ]]; then echo '  [@warning "-27-32-34-35-39"] ' >> all_infer_in_one_file.ml; fi
	$(QUIET)echo $(shell basename $*) | sed -e "s/\b\(.\)/  \u\1/g" >> all_infer_in_one_file.ml
	$(QUIET)echo "  : " >> all_infer_in_one_file.ml
	$(QUIET)echo $(shell basename $*) | sed -e "s/\b\(.\)/  \u\1/g" >> all_infer_in_one_file.ml
	$(QUIET)echo "  = struct " >> all_infer_in_one_file.ml
	$(QUIET)echo '# 1 "$*.ml"' >> all_infer_in_one_file.ml
	cat ../$*.ml >> all_infer_in_one_file.ml
	$(QUIET)echo "  end " >> all_infer_in_one_file.ml
	$(QUIET)echo "(* END OF MODULE $*.ml *)" >> all_infer_in_one_file.ml
	$(QUIET)echo >> all_infer_in_one_file.ml

$(ml_src_files_without_mli:.ml=.cmx):
#	again mostly duplicated from above
	$(QUIET)echo "(* START OF MODULE $(@) *)" >> all_infer_in_one_file.ml
	$(QUIET)echo "  module " >> all_infer_in_one_file.ml
	$(QUIET)if [[ $@ =~ (atd|deadcode)/ ]]; then echo '  [@warning "-27-32-34-35-39"] ' >> all_infer_in_one_file.ml; fi
	$(QUIET)echo $(shell basename $@ .cmx) | sed -e "s/\b\(.\)/  \u\1/g" >> all_infer_in_one_file.ml
	$(QUIET)echo "  = struct " >> all_infer_in_one_file.ml
	$(QUIET)echo "# 1 \"$$(echo $@ | sed -e 's/\.cmx$$/.ml/')\"" >> all_infer_in_one_file.ml
	cat ../$$(echo $@ | sed -e "s/\.cmx$$/.ml/") >> all_infer_in_one_file.ml
	$(QUIET)echo "  end " >> all_infer_in_one_file.ml
	$(QUIET)echo "(* END OF MODULE $@ *)" >> all_infer_in_one_file.ml
	$(QUIET)echo >> all_infer_in_one_file.ml

-include .depend

.PHONY: all_infer_in_one_file.ml
all_infer_in_one_file.ml: backend/infer.cmx unit/inferunit.cmx facebook/InferCreateTraceViewLinks.cmx
	$(QUIET)echo "see results in all_infer_in_one_file.ml"

.PHONY: detect_dead_code
detect_dead_code:
	$(MAKE) clean
#	needed to get jbuild generated, and the generated code for the lexers and parsers in ../_build
	$(MAKE) -C .. byte
#	copy generated source files from ../_build
	for file in $(ml_src_files_from_mlly); do \
	  set +e; \
	  [ -f "../_build/default/$$(basename $$file .mly).ml" ] && \
	    $(COPY) ../_build/default/$$(basename $$file .mly).ml .; \
	  [ -f "../_build/default/$$(basename $$file .mly).mli" ] && \
	    $(COPY) ../_build/default/$$(basename $$file .mly).mli .; \
	  [ -f "../_build/default/$$(basename $$file .mll).ml" ] && \
	    $(COPY) ../_build/default/$$(basename $$file .mll).ml .; \
	  [ -f "../_build/default/$$(basename $$file .mll).mli" ] && \
	    $(COPY) ../_build/default/$$(basename $$file .mll).mli .; \
	  set -e; \
	done
	$(REMOVE) all_infer_in_one_file.ml
	$(MAKE) depend
#	Need to be sequential to avoid getting a garbled file. Need to re-include .depend as it may
#	have changed, so run another `make`.
	$(MAKE) -j 1 all_infer_in_one_file.ml
#	build and get dead code warnings
	jbuilder build ../_build/default/deadcode/all_infer_in_one_file.bc
#	be paranoid about cleaning because we do not want to include infer_in_one_file into infer by
#	accident and I don't know enough jbuilder to be positive that it won't happen
	$(MAKE) clean

.PHONY: clean
clean:
	$(REMOVE) .depend *.ml *.mli jbuild
	touch all_infer_in_one_file.mli all_infer_in_one_file.ml
