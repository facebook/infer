# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

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
# ocamllex and menhir files are generated inside ../_build by dune, and copied here by this
# Makefile.
#
# ocamldepend is used to `cat` the source files in the right order into all_infer_in_one_file.ml.
#
# Beware that this is mostly a terrible hack.

ROOT_DIR = ../../..
include $(ROOT_DIR)/Makefile.config

INFER_BUILD_DIR = ../_build/test

ALL_INFER_IN_ONE_FILE_ML = all_infer_in_one_file.ml

default: detect_dead_code

ml_src_files_from_mlly:=$(shell find .. -not -path "../*stubs*" -regex '\.\./[a-zA-Z].*\.ml[ly]')

ml_src_files:=$(shell cd .. && find . -not -path "./*stubs*" -regex '\./[a-zA-Z].*\.mli*')

ml_src_files_without_mli:=$(shell cd .. && for i in $$(find . -not -path "./*stubs*" -regex '\./[a-zA-Z].*\.ml'); do [ -f $${i}i ] || echo $$i; done)

.PHONY: depend
depend:
	cd .. && \
	ocamldep -native \
	  -I IR -I absint -I al -I atd -I backend -I base -I biabduction -I bufferoverrun \
	  -I checkers -I clang -I concurrency -I facebook -I integration -I istd -I java \
	  -I labs -I nullsafe -I pulse -I scuba -I quandary -I topl -I unit -I unit/clang -I unit/nullsafe -I deadcode \
	  -I test_determinator \
	   $(ml_src_files) > deadcode/.depend

# circular dependency... not sure how to fix properly
%.cmi: %.cmx
#	deal with the .ml *and* the .mli at the same time: when all the modules are inlined in one
#	file, you need the module value to be defined before you can refer to it, even in
#	signatures. Because of this, the order given by ocamldep is not enough to avoid "Unbound
#	module MyModule" errors in the case of signatures referring to other modules.
	$(QUIET)echo "(* START OF SIGNATURE $*.mli *)" >> $(ALL_INFER_IN_ONE_FILE_ML)
#	put too many spaces in general but you never know what the contents of the file is;
#	sometimes spaces will be needed
	$(QUIET)echo "include struct  module type " >> $(ALL_INFER_IN_ONE_FILE_ML)
#	suppress some warnings for generated code
	$(QUIET)if [[ $@ =~ (atd|deadcode)/ ]]; then echo '  [@warning "-27-32-34-35-39"] ' >> $(ALL_INFER_IN_ONE_FILE_ML); fi
#	compute module name from file name: capitalize first letter
	$(QUIET)echo $(shell basename $*) | $(GNU_SED) -e "s/\b\(.\)/  \u\1/g" >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "  = sig " >> $(ALL_INFER_IN_ONE_FILE_ML)
#	pre-processor directive to get errors in the original files and not in all_infer_in_one_file.ml
	$(QUIET)echo '# 1 "$*.mli"' >> $(ALL_INFER_IN_ONE_FILE_ML)
	cat ../$*.mli >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "  end end " >> $(ALL_INFER_IN_ONE_FILE_ML)
#	silence "unused module" warnings for executables
	$(QUIET)if [ $@ = "infer.cmi" ] \
	        || [ $@ = "unit/inferunit.cmi" ] \
	        || [ $@ = "facebook/InferCreateTraceViewLinks.cmi" ] \
	        ; then \
	  echo '[@warning "-60"] ' >> $(ALL_INFER_IN_ONE_FILE_ML); \
	fi
	$(QUIET)echo "(* END OF SIGNATURE $*.mli *)" >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo >> $(ALL_INFER_IN_ONE_FILE_ML)
#	lots of duplication from above, sorry
	$(QUIET)echo "(* START OF MODULE $*.ml *)" >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "include struct  module " >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)if [[ $@ =~ (atd|deadcode)/ ]]; then echo '  [@warning "-27-32-34-35-39"] ' >> $(ALL_INFER_IN_ONE_FILE_ML); fi
	$(QUIET)echo $(shell basename $*) | $(GNU_SED) -e "s/\b\(.\)/  \u\1/g" >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "  : " >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo $(shell basename $*) | $(GNU_SED) -e "s/\b\(.\)/  \u\1/g" >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "  = struct " >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo '# 1 "$*.ml"' >> $(ALL_INFER_IN_ONE_FILE_ML)
	cat ../$*.ml >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "  end end" >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "(* END OF MODULE $*.ml *)" >> $(ALL_INFER_IN_ONE_FILE_ML)
#	silence "unused module" warnings for executables
	$(QUIET)if [ $@ = "infer.cmi" ] \
	        || [ $@ = "unit/inferunit.cmi" ] \
	        || [ $@ = "facebook/InferCreateTraceViewLinks.cmi" ] \
	        ; then \
	  echo '[@warning "-60"] ' >> $(ALL_INFER_IN_ONE_FILE_ML);\
	fi
	$(QUIET)echo >> $(ALL_INFER_IN_ONE_FILE_ML)

$(ml_src_files_without_mli:.ml=.cmx):
#	again mostly duplicated from above
	$(QUIET)echo "(* START OF MODULE $(@) *)" >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "  module " >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)if [[ $@ =~ (atd|deadcode)/ ]]; then echo '  [@warning "-27-32-34-35-39"] ' >> $(ALL_INFER_IN_ONE_FILE_ML); fi
	$(QUIET)echo $(shell basename $@ .cmx) | $(GNU_SED) -e "s/\b\(.\)/  \u\1/g" >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "  = struct " >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "# 1 \"$$(echo $@ | $(GNU_SED) -e 's/\.cmx$$/.ml/')\"" >> $(ALL_INFER_IN_ONE_FILE_ML)
	cat ../$$(echo $@ | $(GNU_SED) -e "s/\.cmx$$/.ml/") >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "  end " >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "(* END OF MODULE $@ *)" >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo >> $(ALL_INFER_IN_ONE_FILE_ML)

%.cmx:
	$(QUIET)echo skipping '$@'

-include .depend

# Concatenate all source files of infer into a single file. Assume that all source files are
# available (in particular generated ones) and .depend has been created by ocamldep. Depend on the
# root .cmx to include all the code. Any code not used in the construction of these "root .cmx" will
# be considered dead.
.PHONY: flatten_infer
flatten_infer: infer.cmx unit/inferunit.cmx facebook/InferCreateTraceViewLinks.cmx
	$(QUIET)echo "see results in $(ALL_INFER_IN_ONE_FILE_ML)"

.PHONY: detect_dead_code
detect_dead_code:
	$(MAKE) clean
#	create a dummy implementation file to keep dune happy, as we are about to generate the
#	dune file for this directory
	touch $(ALL_INFER_IN_ONE_FILE_ML) $(ALL_INFER_IN_ONE_FILE_ML:.ml=.mli)
#	needed to get dune generated, and the generated code for the lexers and parsers in ../_build
	$(MAKE) GENERATED_DUNES=deadcode/dune -C .. test
#	copy generated source files from ../_build
	for file in $(ml_src_files_from_mlly); do \
	  set +e; \
	  [ -f "$(INFER_BUILD_DIR)/$$(basename $$file .mly).ml" ] && \
	    $(COPY) $(INFER_BUILD_DIR)/$$(basename $$file .mly).ml .; \
	  [ -f "$(INFER_BUILD_DIR)/$$(basename $$file .mly).mli" ] && \
	    $(COPY) $(INFER_BUILD_DIR)/$$(basename $$file .mly).mli .; \
	  [ -f "$(INFER_BUILD_DIR)/$$(basename $$file .mll).ml" ] && \
	    $(COPY) $(INFER_BUILD_DIR)/$$(basename $$file .mll).ml .; \
	  [ -f "$(INFER_BUILD_DIR)/$$(basename $$file .mll).mli" ] && \
	    $(COPY) $(INFER_BUILD_DIR)/$$(basename $$file .mll).mli .; \
	  set -e; \
	done
	$(MAKE) depend
#	Need to be sequential to avoid getting a garbled file. Need to re-include .depend as it may
#	have changed. For both of these reasons, run another `make`.
#	Create a temp file so that the build doesn't break if this step gets interrupted.
	tmp_file=$$(mktemp -t all_infer_in_one_file_XXXXX.ml); \
	$(MAKE) -j 1 ALL_INFER_IN_ONE_FILE_ML="$$tmp_file" flatten_infer; \
	mv "$$tmp_file" $(ALL_INFER_IN_ONE_FILE_ML)
#	build and get dead code warnings; clean in case of errors so as not to leave rubbish around
	if ! dune build $(INFER_BUILD_DIR)/deadcode/all_infer_in_one_file.bc; then \
	  $(MAKE) clean; \
	  exit 1; \
	fi
#	be paranoid about cleaning because we do not want to include infer_in_one_file into infer by
#	accident and I don't know enough dune to be positive that it won't happen
	$(MAKE) clean

.PHONY: clean
clean:
	$(REMOVE) .depend *.ml *.mli dune
