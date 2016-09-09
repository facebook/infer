# Copyright (c) 2013 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = ../..
include $(ROOT_DIR)/Makefile.config

#### Global declarations ####

INFER_BUILD_DIR = $(BUILD_DIR)/infer
LLVM_BUILD_DIR = $(BUILD_DIR)/llvm
ANNOT_DIR = $(SRC_DIR)/_build
ETC_DIR = $(INFER_DIR)/etc

ATDGEN_SUFFIXES = _t.ml _t.mli _j.ml _j.mli

#### ocamlbuild options ####

OCAML_FATAL_WARNINGS = +5+6+8+10+11+12+18+19+20+23+26+29+27+32+33+34+35+37+38+39+50

OCAMLBUILD_OPTIONS = \
                     -r \
                     -cflags -g -lflags -g \
                     -cflags -short-paths \
                     -cflags -safe-string \
                     -cflags -principal \
                     -cflags -strict-formats \
                     -cflags -strict-sequence \
                     -cflags -w,$(OCAML_FATAL_WARNINGS)-4-9-40-41-42-45-48 \
                     -tag-line "<*{clang/clang_ast_*,backend/jsonbug_*,checkers/stacktree_*}>: warn(-27-32-35-39)" \
                     -pkgs atdgen,extlib,oUnit,str,unix,yojson,zip

ifeq ($(ENABLE_OCAML_ANNOT),yes)
OCAMLBUILD_OPTIONS += -cflags -annot
endif

ifeq ($(ENABLE_OCAML_BINANNOT),yes)
OCAMLBUILD_OPTIONS += -cflags -bin-annot
endif

ifneq (,$(findstring s,$(MAKEFLAGS)))
OCAMLBUILD_OPTIONS += -quiet
endif

#### Backend declarations ####

INFER_MAIN = backend/infer
INFERANALYZE_MAIN = backend/InferAnalyze

#### Checkers declarations ####

STACKTREE_ATDGEN_STUB_BASE = checkers/stacktree
STACKTREE_ATDGEN_STUB_ATD = $(STACKTREE_ATDGEN_STUB_BASE).atd
STACKTREE_ATDGEN_STUBS = $(addprefix $(STACKTREE_ATDGEN_STUB_BASE), $(ATDGEN_SUFFIXES))

#### InferPrint declarations ####

INFERPRINT_ATDGEN_STUB_BASE = backend/jsonbug
INFERPRINT_ATDGEN_STUB_ATD = $(INFERPRINT_ATDGEN_STUB_BASE).atd
INFERPRINT_ATDGEN_STUBS = $(addprefix $(INFERPRINT_ATDGEN_STUB_BASE), $(ATDGEN_SUFFIXES))

INFERPRINT_MAIN = backend/InferPrint

### InferUnit declarations ###

UNIT_SOURCES = unit

INFERUNIT_MAIN = $(UNIT_SOURCES)/inferunit

#### Infer integration declarations ####
INTEGRATION_SOURCES = integration

BUCK_COMPILATION_DATABASE_MAIN = $(INTEGRATION_SOURCES)/BuckCompilationDatabase

#### Java declarations ####

JAVA_OCAMLBUILD_OPTIONS = -pkgs javalib,ptrees,sawja

JAVA_SOURCES = java

INFERJAVA_MAIN = $(JAVA_SOURCES)/jMain

#### Clang declarations ####

CLANG_SOURCES = clang
INFERCLANG_MAIN = $(CLANG_SOURCES)/cMain

FCP_CLANG_OCAML_BUILD_DIR = $(FCP_CLANG_OCAML_DIR)/build

CLANG_PLUGIN_BINARIES = $(addprefix $(FCP_CLANG_OCAML_BUILD_DIR), \
                          clang_ast_converter clang_ast_named_decl_printer)

CLANG_AST_BASE_NAME = clang_ast
CLANG_ATDGEN_STUB_BASE = $(CLANG_SOURCES)/$(CLANG_AST_BASE_NAME)
CLANG_ATDGEN_STUB_ATD = $(FCP_CLANG_OCAML_BUILD_DIR)/$(CLANG_AST_BASE_NAME).atd
CLANG_ATDGEN_SUFFIXES = _t.ml _t.mli _b.ml _b.mli _j.ml _j.mli _v.ml _v.mli
CLANG_ATDGEN_STUBS = $(addprefix $(CLANG_ATDGEN_STUB_BASE), $(CLANG_ATDGEN_SUFFIXES))

FCP_CLANG_AST_PROJ = $(addprefix $(FCP_CLANG_OCAML_BUILD_DIR)/, \
                       clang_ast_proj.ml clang_ast_proj.mli)
FCP_CLANG_AST_MAIN = $(addprefix $(FCP_CLANG_OCAML_DIR)/, \
                       clang_ast_visit.ml clang_ast_main.ml)
FCP_FILES_TO_MIRROR = $(FCP_CLANG_AST_PROJ) $(FCP_CLANG_AST_MAIN)
INFER_CLANG_FCP_MIRRORED_FILES = $(addprefix $(CLANG_SOURCES)/, $(notdir $(FCP_FILES_TO_MIRROR)))

CLANG_BINIOU_DICT = $(ETC_DIR)/clang_ast.dict

#### LLVM declarations ####

LLVM_SOURCES = llvm
MENHIR_SOURCES = menhir
INFERLLVM_MAIN = $(LLVM_SOURCES)/lMain
INFERLLVM_BIN = $(BIN_DIR)/InferLLVM

#### scripts declarations ####

SCRIPT_SOURCES = scripts

CHECKCOPYRIGHT_BIN = $(SCRIPT_DIR)/checkCopyright
CHECKCOPYRIGHT_MAIN = $(SCRIPT_SOURCES)/checkCopyright

STATSAGGREGATOR_BIN = $(BIN_DIR)/InferStatsAggregator
STATSAGGREGATOR_MAIN = $(SCRIPT_SOURCES)/StatsAggregator

#### End of declarations ####

ifeq ($(IS_FACEBOOK_TREE),yes)
EXTRA_DEPS = facebook facebook/checkers facebook/checkers/graphql facebook/scripts
else
EXTRA_DEPS = opensource
endif

DEPENDENCIES = IR backend checkers eradicate harness quandary $(EXTRA_DEPS)

# ocamlbuild command with options common to all build targets
OCAMLBUILD_BASE = rebuild $(OCAMLBUILD_OPTIONS) -j $(NCPU) $(addprefix -I , $(DEPENDENCIES))

# ocamlbuild with options necessary to build all targets at once, regardless of configure flags
OCAMLBUILD_ALL = $(OCAMLBUILD_BASE) $(JAVA_OCAMLBUILD_OPTIONS)

# list of ocamlbuild targets common to all build targets -- native version
INFER_BASE_TARGETS = \
  $(INFER_MAIN).native \
  $(INFERANALYZE_MAIN).native \
  $(INFERPRINT_MAIN).native \
  $(INFERUNIT_MAIN).native \
  $(CHECKCOPYRIGHT_MAIN).native \
  $(STATSAGGREGATOR_MAIN).native \
  $(INFERUNIT_MAIN).native

INFER_ALL_TARGETS = $(INFER_BASE_TARGETS) \
  $(INFERJAVA_MAIN).native \
  $(INFERCLANG_MAIN).native \
  $(INFERLLVM_MAIN).native \
	$(BUCK_COMPILATION_DATABASE_MAIN).native

# configure-aware ocamlbuild commands and targets
OCAMLBUILD_CONFIG = $(OCAMLBUILD_BASE)
INFER_CONFIG_TARGETS = $(INFER_BASE_TARGETS)

ifeq ($(BUILD_JAVA_ANALYZERS),yes)
OCAMLBUILD_CONFIG += $(JAVA_OCAMLBUILD_OPTIONS)
INFER_CONFIG_TARGETS += $(INFERJAVA_MAIN).native
DEPENDENCIES += java
endif
ifeq ($(BUILD_C_ANALYZERS),yes)
INFER_CONFIG_TARGETS += $(INFERCLANG_MAIN).native
INFER_CONFIG_TARGETS += $(BUCK_COMPILATION_DATABASE_MAIN).native
DEPENDENCIES += clang
endif
ifeq ($(BUILD_LLVM_ANALYZERS),yes)
INFER_CONFIG_TARGETS += $(INFERLLVM_MAIN).native
DEPENDENCIES += llvm
endif

.PHONY: all infer init version sanitize clean toplevel

all: infer

infer: init $(STACKTREE_ATDGEN_STUBS) $(INFERPRINT_ATDGEN_STUBS)
	$(OCAMLBUILD_CONFIG) -build-dir $(INFER_BUILD_DIR) $(INFER_CONFIG_TARGETS)
	$(COPY) $(INFER_BUILD_DIR)/$(INFER_MAIN).native $(INFER_BIN)
	$(COPY) $(INFER_BUILD_DIR)/$(INFERANALYZE_MAIN).native $(INFERANALYZE_BIN)
	$(COPY) $(INFER_BUILD_DIR)/$(INFERPRINT_MAIN).native $(INFERPRINT_BIN)
	$(COPY) $(INFER_BUILD_DIR)/$(CHECKCOPYRIGHT_MAIN).native $(CHECKCOPYRIGHT_BIN)
	$(COPY) $(INFER_BUILD_DIR)/$(STATSAGGREGATOR_MAIN).native $(STATSAGGREGATOR_BIN)
	$(COPY) $(INFER_BUILD_DIR)/$(INFERUNIT_MAIN).native $(INFERUNIT_BIN)
ifeq ($(BUILD_LLVM_ANALYZERS),yes)
	$(COPY) $(INFER_BUILD_DIR)/$(INFERLLVM_MAIN).native $(INFERLLVM_BIN)
endif
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
	$(COPY) $(INFER_BUILD_DIR)/$(INFERJAVA_MAIN).native $(INFERJAVA_BIN)
endif
ifeq ($(BUILD_C_ANALYZERS),yes)
	$(COPY) $(INFER_BUILD_DIR)/$(INFERCLANG_MAIN).native $(INFERCLANG_BIN)
	$(COPY) $(INFER_BUILD_DIR)/$(BUCK_COMPILATION_DATABASE_MAIN).native $(INFER_BUCK_COMPILATION_DATABASE_BIN)
endif
ifeq ($(ENABLE_OCAML_ANNOT),yes)
	rsync -a --include '*/' --include '*.annot' --exclude '*' $(INFER_BUILD_DIR)/ $(ANNOT_DIR)/
endif

ifeq ($(BUILD_C_ANALYZERS),yes)
infer: $(CLANG_ATDGEN_STUBS) $(INFER_CLANG_FCP_MIRRORED_FILES) $(CLANG_BINIOU_DICT)
endif
ifeq ($(BUILD_LLVM_ANALYZERS),yes)
infer: $(LLVM_SOURCES)/lLexer.ml $(LLVM_SOURCES)/lParser.ml
endif

$(LLVM_SOURCES)/lLexer.ml $(LLVM_SOURCES)/lParser.ml: init
	$(OCAMLBUILD_BASE) -build-dir $(LLVM_BUILD_DIR) -I $(LLVM_SOURCES) -use-menhir $(MENHIR_SOURCES)/$(@F)
	$(COPY) $(LLVM_BUILD_DIR)/$(MENHIR_SOURCES)/$(@F) $@

$(LLVM_SOURCES)/lLexer.ml: $(MENHIR_SOURCES)/lLexer.mll
# avoid race condition between concurrently running ocamlbuild instances by making the parser depend
# on the lexer
$(LLVM_SOURCES)/lParser.ml: $(MENHIR_SOURCES)/lParser.mly $(LLVM_SOURCES)/lLexer.ml

byte: init $(STACKTREE_ATDGEN_STUBS) $(INFERPRINT_ATDGEN_STUBS) $(CLANG_ATDGEN_STUBS) $(INFER_CLANG_FCP_MIRRORED_FILES) \
  $(LLVM_SOURCES)/lLexer.ml $(LLVM_SOURCES)/lParser.ml
	$(OCAMLBUILD_ALL) -build-dir $(INFER_BUILD_DIR) $(INFER_ALL_TARGETS:.native=.byte)

# to build only the single module <Module> (and its dependencies) with extra flags execute:
#   make MFLAGS=<flags> M=<Module>.cm{o,x} module
# for example, to build the assembly for the Ident module, execute
#   make MFLAGS="-ocamlopt 'ocamlopt -S'" M=Ident.cmx module
M=
MFLAGS=

module: init $(STACKTREE_ATDGEN_STUBS) $(INFERPRINT_ATDGEN_STUBS) $(CLANG_ATDGEN_STUBS) $(INFER_CLANG_FCP_MIRRORED_FILES) \
  $(LLVM_SOURCES)/lLexer.ml $(LLVM_SOURCES)/lParser.ml
	$(OCAMLBUILD_ALL) -build-dir $(INFER_BUILD_DIR) \
          $(MFLAGS) \
	  $(M)

test_build: init $(STACKTREE_ATDGEN_STUBS) $(INFERPRINT_ATDGEN_STUBS) $(CLANG_ATDGEN_STUBS) $(INFER_CLANG_FCP_MIRRORED_FILES) \
  $(LLVM_SOURCES)/lLexer.ml $(LLVM_SOURCES)/lParser.ml
	$(OCAMLBUILD_ALL) -build-dir $(TEST_BUILD_DIR) \
	  -cflags -warn-error,$(OCAML_FATAL_WARNINGS) \
	  $(INFER_ALL_TARGETS:.native=.byte)

$(DEPENDENCIES_DIR)/ocamldot/ocamldot:
	$(MAKE) -C $(DEPENDENCIES_DIR)/ocamldot

roots:=Infer InferAnalyze CMain JMain InferPrint BuckCompilationDatabase

src_dirs:=$(shell find * -type d)
ml_src_files:=$(shell find $(src_dirs) -regex '.*\.ml\(i\)*' -not -path facebook/scripts/eradicate_stats.ml)
re_src_files:=$(shell find $(src_dirs) -regex '.*\.re\(i\)*')
inc_flags:=$(foreach dir,$(src_dirs),-I $(dir))
root_flags:=$(foreach root,$(roots),-r $(root))
mod_dep.dot: $(DEPENDENCIES_DIR)/ocamldot/ocamldot $(ml_src_files) $(re_src_files)
	{ ocamldep.opt $(inc_flags) -ml-synonym .re -mli-synonym .rei $(ml_src_files); \
	  ocamldep.opt $(inc_flags) -ml-synonym .re -mli-synonym .rei -pp refmt $(re_src_files); } \
	| $(DEPENDENCIES_DIR)/ocamldot/ocamldot -lr $(root_flags) \
        > mod_dep.dot

mod_dep.pdf: mod_dep.dot
	dot -Tpdf -o mod_dep.pdf mod_dep.dot

roots_grep_regex:=$(foreach root,$(roots),-e $(root)$$)
dirs_find_regex:=$(foreach dir, $(DEPENDENCIES),-path "./$(dir)/*" -o)
toplevel: init $(STACKTREE_ATDGEN_STUBS) $(INFERPRINT_ATDGEN_STUBS) $(CLANG_ATDGEN_STUBS) $(INFER_CLANG_FCP_MIRRORED_FILES)
	# We need to pack all infer modules into another module to avoid name clashes
	# with some of them coming from ocaml libraries (Ident for example).
	# To do that, we generate .mlpack file with source files. Steps:
	# 1. find all interesting .re and .ml files - they need to be in one of
	#    directories listed in $(DEPENDENCIES)
	# 2. remove './' from the beginning of each line
	# 3. remove extension from all files
	# 4. make first letter of filename uppercase to produce valid ocaml module
	# 5. filter out root modules since they run code when loading the module.
	find . \( -name "*.ml" -o -name "*.mly" -o -name "*.mll" -o -name "*.re" \) \
		\( $(dirs_find_regex) -false \) \
		-not \( -path "./unit/*" -o -path "./llvm/*" -o -path "./facebook/scripts/eradicate_stats.ml" -o -name ".#*" \) \
	| cut -c 3- \
	| rev | cut -f 2- -d '.' | rev \
	| awk 'BEGIN { FS = "/"; OFS = "/" } ; {$$NF=toupper(substr($$NF,1,1))substr($$NF,2); print $$0}' \
	| grep -v $(roots_grep_regex) > toplevel.mlpack
	$(OCAMLBUILD_ALL) -build-dir $(INFER_BUILD_DIR) toplevel.cmo

define gen_atdgen_rules
# generate files using atdgen
# parameters:
#   1. the .atd file to generate .ml{,i} files from
#   2. the base name of .ml{,i} files
#   3. the type of files to generate: b, j, t, or v

$(2)_$(3).ml $(2)_$(3).mli: $(1)
	$(ATDGEN) -$(3) $$< -o $(2)

# the .ml depends on the corresponding .mli to avoid running atdgen
# twice during parallel builds
$(2)_$(3).ml: $(2)_$(3).mli
endef

$(foreach atd_type,j t,\
    $(eval \
        $(call gen_atdgen_rules,$(INFERPRINT_ATDGEN_STUB_ATD),$(INFERPRINT_ATDGEN_STUB_BASE),$(atd_type))))

$(foreach atd_type,j t,\
		$(eval \
				$(call gen_atdgen_rules,$(STACKTREE_ATDGEN_STUB_ATD),$(STACKTREE_ATDGEN_STUB_BASE),$(atd_type))))

# rebuild the artifacts of the AST files whenever they're upated in FCP
$(foreach atd_type,b j t v,\
    $(eval \
        $(call gen_atdgen_rules,$(CLANG_ATDGEN_STUB_ATD),$(CLANG_ATDGEN_STUB_BASE),$(atd_type))))


define mirror_fcp_file
$(CLANG_SOURCES)/$(notdir $(1)): $(1)
	$(INSTALL) -m 644 -C $$< $$@
endef

$(foreach file, $(FCP_FILES_TO_MIRROR), $(eval $(call mirror_fcp_file,$(file))))


$(CLANG_BINIOU_DICT): $(CLANG_ATDGEN_STUB_ATD)
# overapproximation of the words we need in the biniou dictionary
# the long litany of symbols is [:punct:] minus "_-'"
	tr -s '[*!"#\$%&\(\)\+,\\\.\/:;<=>\?@\[\\\\]^`\{|\}~[:space:]]' '\n' \
	  < $< \
	  | sort | uniq  \
	  > $@

init: sanitize version $(BUILD_DIR)

sanitize:
ifneq ($(wildcard $(BUILD_DIR)/sanitize.sh),)
	$(BUILD_DIR)/sanitize.sh
endif

version: backend/version.ml.in Makefile
	TMPFILE=$$(mktemp backend/version.ml.tmp.XXXX); \
	INFER_GIT_COMMIT=$$(git --work-tree=$(ROOT_DIR) --git-dir=$(ROOT_DIR)/.git rev-parse --short HEAD || printf "unknown"); \
	INFER_GIT_BRANCH=$$(git --work-tree=$(ROOT_DIR) --git-dir=$(ROOT_DIR)/.git rev-parse --abbrev-ref HEAD || printf "unknown"); \
	sed \
	  -e 's|@INFER_MAJOR[@]|$(INFER_MAJOR)|g' \
	  -e 's|@INFER_MINOR[@]|$(INFER_MINOR)|g' \
	  -e 's|@INFER_PATCH[@]|$(INFER_PATCH)|g' \
	  -e 's|@IS_RELEASE_TREE[@]|$(IS_RELEASE_TREE)|g' \
	  -e "s|@INFER_GIT_COMMIT[@]|$$INFER_GIT_COMMIT|g" \
	  -e "s|@INFER_GIT_BRANCH[@]|$$INFER_GIT_BRANCH|g" \
	  $< > "$$TMPFILE"; \
	$(INSTALL) -m 644 -C "$$TMPFILE" backend/version.ml; \
	rm -f "$$TMPFILE"

$(BUILD_DIR):
	$(MKDIR_P) $(BUILD_DIR)

test_clean:
	$(REMOVE_DIR) $(TEST_BUILD_DIR)

clean:
	$(REMOVE_DIR) $(BUILD_DIR)
	$(REMOVE) $(ETC_DIR)/clang_ast.dict
ifeq ($(ENABLE_OCAML_ANNOT),yes)
	$(REMOVE_DIR) $(ANNOT_DIR)
endif
ifeq ($(BUILD_LLVM_ANALYZERS),yes)
	$(REMOVE) $(LLVM_SOURCES)/lLexer.ml $(LLVM_SOURCES)/lParser.ml
endif
	$(REMOVE) backend/version.ml
	$(REMOVE) backend/version.ml.tmp.*
	$(REMOVE) backend/jsonbug_{j,t}.ml{,i}
	$(REMOVE) checkers/stacktree_{j,t}.ml{,i}
	$(REMOVE) $(INFER_BIN) $(INFERANALYZE_BIN) $(INFERPRINT_BIN) $(STATSAGGREGATOR_BIN)
	$(REMOVE) $(INFERJAVA_BIN) $(INFERCLANG_BIN) $(INFERLLVM_BIN)
	$(REMOVE) $(INFERUNIT_BIN) $(CHECKCOPYRIGHT_BIN) $(INFER_BUCK_COMPILATION_DATABASE_BIN)
	$(REMOVE) $(CLANG_ATDGEN_STUBS)
	$(REMOVE) $(INFER_CLANG_FCP_MIRRORED_FILES)
	$(REMOVE) mod_dep.dot
	$(REMOVE) mod_dep.pdf

# convert to reason
%.re : %.ml
	refmt -assume-explicit-arity -heuristics-file unary.txt -parse ml -print re $< > $*.re

%.rei : %.mli
	refmt -assume-explicit-arity -heuristics-file unary.txt -parse ml -print re $< > $*.rei
