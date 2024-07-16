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

INFER_BUILD_DIR = $(INFER_DIR)/_build/default/src
DEADCODE_DIR = $(INFER_DIR)/src/deadcode

ALL_INFER_IN_ONE_FILE_ML = all_infer_in_one_file.ml
ALL_ML_FILES = all_ml_files
ALL_MLI_FILES = all_mli_files
ALL_ML_FILES_COPIED = all_ml_files_copied
ALL_MLI_FILES_COPIED = all_mli_files_copied

default: detect_dead_code

# Annoying find being different on OSX/BSD and GNU/Linux
ifeq ($(PLATFORM),Linux)
DEPTH_ONE = -mindepth 1 -maxdepth 1
else
DEPTH_ONE = -depth 1
endif

# ./ is necessary for find to work correctly.
# Note that we run find under _build directory. Since we copy some
# sources from subfolders to src/ folder to avoid duplicates we use
# $(DEPTH_ONE) and iteration over main and library folders.
LIBRARY_FOLDERS = . ./IR  ./textual ./python ./absint ./atd ./backend ./base ./biabduction ./bufferoverrun ./checkers ./clang ./clang/unit ./concurrency ./cost ./erlang ./integration ./istd ./java ./labs ./dotnet ./pulse ./scripts ./topl ./unit
INCLUDE_FOLDERS = -I IR -I textual -I textual/unit -I python -I python/unit -I absint -I atd -I backend -I base -I biabduction -I bufferoverrun -I checkers -I clang -I clang/unit -I concurrency -I cost -I erlang -I integration -I istd -I java -I labs -I pulse -I scripts -I topl -I unit

ml_src_files = $(shell \
  cd $(INFER_BUILD_DIR); \
  for d in $(LIBRARY_FOLDERS); do \
  [ -d $$d ] && echo $$(find $$d $(DEPTH_ONE) -regex '\./[a-zA-Z].*\.ml' \( -not -regex '.*\.pp\.ml' \) \
                | sed 's/^\.\///'); \
  done)

mli_src_files = $(shell \
  cd $(INFER_BUILD_DIR); \
  for d in $(LIBRARY_FOLDERS); do \
  [ -d $$d ] && echo $$(find $$d $(DEPTH_ONE) -regex '\./[a-zA-Z].*\.mli' \( -not -regex '.*\.pp\.mli' \) \
                | sed 's/^\.\///'); \
  done)

ml_src_files_without_mli:=$(shell \
  cd $(INFER_BUILD_DIR); \
  for i in $(ml_src_files); do [ -f $${i}i ] || echo $$i; done)

.PHONY: depend
depend:
	cd $(INFER_BUILD_DIR) && \
	ocamldep -native -one-line \
	  $(INCLUDE_FOLDERS) \
	  $(ml_src_files) $(mli_src_files) > $(DEADCODE_DIR)/.depend
#	For each module foo, union the dependencies of foo.cmx and foo.cmi together. This is because
#	flatten_infer will include a .ml file *together* with its corresponding .mli file, so will
#	need the dependencies of *both* of these files to have been dealt with before we include
#	*either* of these files.
#
#	This proceeds in two steps: first remove any "foo.cmi" dep from "foo.cmx", then rename all
#	.cmi to .cmx. The first step avoids introducing a circular dependency in the second step.
	$(GNU_SED) -i .depend \
	  -e 's/^\([^ :]\+\)\.cmx[ ]*: \(.*\)\1\.cmi/\1.cmx: \2/' \
	  -e 's/\.cmi\b/.cmx/g'

%.cmx:
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
	cat $(INFER_BUILD_DIR)/$*.mli >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "  end end " >> $(ALL_INFER_IN_ONE_FILE_ML)
#	silence "unused module" warnings for executables
	$(QUIET)if [ $@ = "infer.cmx" ] \
	        || [ $@ = "inferunit.cmx" ] \
	        ; then \
	  echo '[@warning "-unused-module"] ' >> $(ALL_INFER_IN_ONE_FILE_ML); \
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
	cat $(INFER_BUILD_DIR)/$*.ml >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "  end end" >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "(* END OF MODULE $*.ml *)" >> $(ALL_INFER_IN_ONE_FILE_ML)
#	silence "unused module" warnings for executables
	$(QUIET)if [ $@ = "infer.cmx" ] \
	        || [ $@ = "inferunit.cmx" ] \
	        || [ $@ = "scripts/checkCopyright.cmx" ] \
	        ; then \
	  echo '[@warning "-unused-module"] ' >> $(ALL_INFER_IN_ONE_FILE_ML);\
	fi
	$(QUIET)echo >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "$*.mli" >> $(ALL_MLI_FILES_COPIED)
	$(QUIET)echo "$*.ml" >> $(ALL_ML_FILES_COPIED)

$(ml_src_files_without_mli:.ml=.cmx):
#	again mostly duplicated from above
	$(QUIET)echo "(* START OF MODULE $(@) *)" >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "  module " >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)if [[ $@ =~ (atd|deadcode)/ ]]; then echo '  [@warning "-27-32-34-35-39"] ' >> $(ALL_INFER_IN_ONE_FILE_ML); fi
	$(QUIET)echo $(shell basename $@ .cmx) | $(GNU_SED) -e "s/\b\(.\)/  \u\1/g" >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "  = struct " >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "# 1 \"$$(echo $@ | $(GNU_SED) -e 's/\.cmx$$/.ml/')\"" >> $(ALL_INFER_IN_ONE_FILE_ML)
	cat $(INFER_BUILD_DIR)/$$(echo $@ | $(GNU_SED) -e "s/\.cmx$$/.ml/") >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "  end " >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo "(* END OF MODULE $@ *)" >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo >> $(ALL_INFER_IN_ONE_FILE_ML)
	$(QUIET)echo $@ | $(GNU_SED) -e 's/\.cmx$$/.ml/' >> $(ALL_ML_FILES_COPIED)

-include .depend

# Concatenate all source files of infer into a single file. Assume that all source files are
# available (in particular generated ones) and .depend has been created by ocamldep. Depend on the
# root .cmx to include all the code. Any code not used in the construction of these "root .cmx" will
# be considered dead.
.PHONY: flatten_infer
flatten_infer: infer.cmx inferunit.cmx scripts/checkCopyright.cmx
	$(QUIET)echo "see results in $(ALL_INFER_IN_ONE_FILE_ML)"

# copy dune.in to dune only when needed to avoid dune trying to build
# all_infer_in_one_file as part of the rest of the build
dune: dune.in
	$(QUIET)$(INSTALL_DATA) $< $@

.PHONY: detect_dead_code
detect_dead_code: dune
#	create a dummy implementation file to keep dune happy, as we are about to generate the
#	dune file for this directory
	printf "" > $(ALL_INFER_IN_ONE_FILE_ML)
	printf "" > $(ALL_INFER_IN_ONE_FILE_ML:.ml=.mli)
#	needed to get the generated code for the lexers and parsers in ../_build
	$(MAKE) -C .. check
	$(MAKE) depend
#	Need to be sequential to avoid getting a garbled file. Need to re-include .depend as it may
#	have changed. For both of these reasons, run another `make`.
#	Create a temp file so that the build doesn't break if this step gets interrupted.
	tmp_file=$$(mktemp -t all_infer_in_one_file_XXXXX.ml); \
	tmp_file_copied=$$(mktemp -t all_ml_files_copied_XXXXX); \
	$(MAKE) -j 1 ALL_INFER_IN_ONE_FILE_ML="$$tmp_file" ALL_ML_FILES_COPIED="$$tmp_file_copied" flatten_infer; \
	mv "$$tmp_file" $(ALL_INFER_IN_ONE_FILE_ML); \
	sort -u "$$tmp_file_copied" > $(ALL_ML_FILES_COPIED); \
	rm "$$tmp_file_copied"
#	build and get dead code warnings
	$(DUNE_BUILD) @check
	$(REMOVE) dune
	$(MAKE) -j 1 detect_dead_src_file

.PHONY: detect_dead_src_file
detect_dead_src_file:
	function is_in_block_list { \
	  if [ $$1 = infertop.ml ] \
	  || [ $$1 = infertop.mli ] \
	  || [ $$1 = deadcode/all_infer_in_one_file.ml ] \
	  || [ $$1 = deadcode/all_infer_in_one_file.mli ] \
	  || [[ $$1 =~ .*FrontendStubs.mli?$$ ]] \
	  || [[ $$1 =~ ^labs/ ]] \
	  || [[ $$1 =~ ^llvm/ ]] \
	  || [[ $$1 =~ ^opensource/ ]] \
	  || grep -q -F 'Ppx_inline_test_lib.Runtime.exit' $(INFER_BUILD_DIR)/$$1 \
	  || grep -q -F 'let%test_module' $(INFER_BUILD_DIR)/$$1 ; then \
	    exit 0; \
          else \
	    exit 1; \
	  fi; \
	}; \
	\
	tmp_file=$$(mktemp -t all_ml_files_XXXXX); \
	for i in $(ml_src_files) $(ml_src_files_without_mli); do \
	  (is_in_block_list "$$i") || (echo "$$i" >> "$$tmp_file"); \
	done; \
	sort -u "$$tmp_file" > $(ALL_ML_FILES); \
	rm "$$tmp_file"; \
	\
	tmp_file=$$(mktemp -t all_mli_files_XXXXX); \
	for i in $(mli_src_files); do \
	  (is_in_block_list "$$i") || (echo "$$i" >> "$$tmp_file"); \
	done; \
	sort -u "$$tmp_file" > $(ALL_MLI_FILES); \
	rm "$$tmp_file"; \
	\
	tmp_file=$$(mktemp -t all_ml_files_copied_XXXXX); \
	for i in $$(cat $(ALL_ML_FILES_COPIED)); do \
	  (is_in_block_list "$$i") || (echo "$$i" >> "$$tmp_file"); \
	done; \
	sort -u "$$tmp_file" > $(ALL_ML_FILES_COPIED); \
	rm "$$tmp_file"; \
	\
	tmp_file=$$(mktemp -t all_mli_files_copied_XXXXX); \
	for i in $$(cat $(ALL_MLI_FILES_COPIED)); do \
	  (is_in_block_list "$$i") || (echo "$$i" >> "$$tmp_file"); \
	done; \
	sort -u "$$tmp_file" > $(ALL_MLI_FILES_COPIED); \
	rm "$$tmp_file"
	diff $(ALL_ML_FILES) $(ALL_ML_FILES_COPIED) 1>&2
	diff $(ALL_MLI_FILES) $(ALL_MLI_FILES_COPIED) 1>&2

.PHONY: clean
clean:
	$(REMOVE) .depend *.ml *.mli $(ALL_ML_FILES) $(ALL_MLI_FILES) $(ALL_ML_FILES_COPIED) \
	  $(ALL_MLI_FILES_COPIED) dune
