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
ANNOT_DIR = $(SRC_DIR)/_build
ETC_DIR = $(INFER_DIR)/etc

ATDGEN_SUFFIXES = _t.ml _t.mli _j.ml _j.mli

#### ocamlbuild options ####

OCAML_FATAL_WARNINGS = +3+5+6+8+10+11+12+18+19+20+23+26+29+27+33+34+35+37+38+39+50

# options for ocamlbuild
# Notice that use-ocamlfind is used here to avoid a linker bug in ocamlbuild when using -tag thread
OCAMLBUILD_OPTIONS = \
  -r \
  -use-menhir -menhir 'menhir --explain --strict'\
  -use-ocamlfind \
  -lflags -cclib,-lffi \
  -cflags -g -lflags -g \
  -cflags -short-paths \
  -cflags -safe-string \
  -cflags -principal \
  -cflags -strict-formats \
  -cflags -strict-sequence \
  -cflags -w,$(OCAML_FATAL_WARNINGS)-4-9-32-40-41-42-45-48 \
  -tag-line "<*{clang/clang_ast_*,backend/jsonbug_*,checkers/stacktree_*}>: warn(-27-32-35-39)" \
  -tag-line "<*/{,*/}*.{ml,re}{,i}>: package(ppx_compare)" \
  -tag thread \
  -pkgs atdgen,core,ctypes.foreign,oUnit,str,unix,yojson,zip

ifeq ($(ENABLE_OCAML_ANNOT),yes)
OCAMLBUILD_OPTIONS += -cflags -annot
endif

ifeq ($(ENABLE_OCAML_BINANNOT),yes)
OCAMLBUILD_OPTIONS += -cflags -bin-annot
endif

ifeq ($(ENABLE_OCAMLOPT_CUSTOM_CC),yes)
OCAMLBUILD_OPTIONS += -lflags -cc,$(CC) -cflags -cc,$(CC)
endif

ifneq (,$(findstring s,$(MAKEFLAGS)))
OCAMLBUILD_OPTIONS += -quiet
endif


#### Base declaration ####

C_STUBS_SOURCE = stubs/c/stubs.c
# only one C object for now, the Makefile will need changing if more are added
C_STUBS_OBJ = $(C_STUBS_SOURCE:.c=.o)

#### Backend declarations ####

INFER_MAIN = backend/infer
INFERANALYZE_MAIN = backend/InferAnalyzeExe

#### Checkers declarations ####

STACKTREE_ATDGEN_STUB_BASE = checkers/stacktree
STACKTREE_ATDGEN_STUB_ATD = $(STACKTREE_ATDGEN_STUB_BASE).atd
STACKTREE_ATDGEN_STUBS = $(addprefix $(STACKTREE_ATDGEN_STUB_BASE), $(ATDGEN_SUFFIXES))

#### InferPrint declarations ####

INFERPRINT_ATDGEN_STUB_BASE = backend/jsonbug
INFERPRINT_ATDGEN_STUB_ATD = $(INFERPRINT_ATDGEN_STUB_BASE).atd
INFERPRINT_ATDGEN_STUBS = $(addprefix $(INFERPRINT_ATDGEN_STUB_BASE), $(ATDGEN_SUFFIXES))

INFERPRINT_MAIN = backend/InferPrintExe

### InferUnit declarations ###

UNIT_SOURCES = unit

INFERUNIT_MAIN = $(UNIT_SOURCES)/inferunit

#### Java declarations ####

JAVA_OCAMLBUILD_OPTIONS = -pkgs javalib,ptrees,sawja

JAVA_SOURCES = java

INFERJAVA_MAIN = $(JAVA_SOURCES)/jMain

#### Clang declarations ####

CLANG_SOURCES = clang
INFERCLANG_MAIN = $(CLANG_SOURCES)/InferClang

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

#### scripts declarations ####

SCRIPT_SOURCES = scripts

CHECKCOPYRIGHT_BIN = $(SCRIPT_DIR)/checkCopyright
CHECKCOPYRIGHT_MAIN = $(SCRIPT_SOURCES)/checkCopyright

#### End of declarations ####

ifeq ($(IS_FACEBOOK_TREE),yes)
EXTRA_DEPS = facebook facebook/checkers facebook/checkers/graphql facebook/scripts
else
EXTRA_DEPS = opensource
endif

DEPENDENCIES = IR backend base checkers eradicate harness integration tp/fts quandary $(EXTRA_DEPS)

# ocamlbuild command with options common to all build targets
OCAMLBUILD_BASE = rebuild $(OCAMLBUILD_OPTIONS) -j $(NCPU) $(addprefix -I , $(DEPENDENCIES))

# ocamlbuild with options necessary to build all targets at once, regardless of configure flags
OCAMLBUILD_ALL = $(OCAMLBUILD_BASE) $(JAVA_OCAMLBUILD_OPTIONS)

OCAMLBUILD_BYTE_OPTS = -lflags -custom,$(C_STUBS_OBJ) -I stubs/c

OCAMLBUILD_NATIVE_OPTS = -lflags $(C_STUBS_OBJ) -I stubs/c

# list of ocamlbuild targets common to all build targets -- native version
INFER_BASE_TARGETS = \
  $(C_STUBS_OBJ) \
  $(INFER_MAIN).native \
  $(INFERANALYZE_MAIN).native \
  $(INFERPRINT_MAIN).native \
  $(INFERUNIT_MAIN).native

INFER_ALL_TARGETS = $(INFER_BASE_TARGETS) \
  $(INFERJAVA_MAIN).native \
  $(INFERCLANG_MAIN).native \

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
DEPENDENCIES += clang
endif

export OCAMLFIND_IGNORE_DUPS_IN=$(shell ocamlc -where)/compiler-libs

.PHONY: all
all: infer

.PHONY: infer
infer: init $(STACKTREE_ATDGEN_STUBS) $(INFERPRINT_ATDGEN_STUBS)
	$(OCAMLBUILD_CONFIG) -build-dir $(INFER_BUILD_DIR) $(OCAMLBUILD_NATIVE_OPTS) \
	  $(INFER_CONFIG_TARGETS)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFER_MAIN).native $(INFER_BIN)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFERANALYZE_MAIN).native $(INFERANALYZE_BIN)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFERPRINT_MAIN).native $(INFERPRINT_BIN)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFERUNIT_MAIN).native $(INFERUNIT_BIN)
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFERJAVA_MAIN).native $(INFERJAVA_BIN)
endif
ifeq ($(BUILD_C_ANALYZERS),yes)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFERCLANG_MAIN).native $(INFERCLANG_BIN)
	cd $(INFER_BUILD_DIR) && $(LN_S) -f InferClang InferClang++ && cd -
endif
ifeq ($(ENABLE_OCAML_ANNOT),yes)
	rsync -a --include '*/' --include '*.annot' --exclude '*' $(INFER_BUILD_DIR)/ $(ANNOT_DIR)/
endif

ifeq ($(BUILD_C_ANALYZERS),yes)
infer: $(CLANG_ATDGEN_STUBS) $(INFER_CLANG_FCP_MIRRORED_FILES) $(CLANG_BINIOU_DICT)
endif

.PHONY: byte
byte: init $(STACKTREE_ATDGEN_STUBS) $(INFERPRINT_ATDGEN_STUBS) $(CLANG_ATDGEN_STUBS) $(INFER_CLANG_FCP_MIRRORED_FILES)
	$(OCAMLBUILD_ALL) $(OCAMLBUILD_BYTE_OPTS) -build-dir $(INFER_BUILD_DIR) \
	  $(INFER_ALL_TARGETS:.native=.byte)

# to build only the single module <Module> (and its dependencies) with extra flags execute:
#   make MFLAGS=<flags> M=<Module>.cm{o,x} module
# for example, to build the assembly for the Ident module, execute
#   make MFLAGS="-ocamlopt 'ocamlopt -S'" M=Ident.cmx module
M=
MFLAGS=

.PHONY: module
module: init $(STACKTREE_ATDGEN_STUBS) $(INFERPRINT_ATDGEN_STUBS) $(CLANG_ATDGEN_STUBS) $(INFER_CLANG_FCP_MIRRORED_FILES)
	$(OCAMLBUILD_ALL) -build-dir $(INFER_BUILD_DIR) \
          $(MFLAGS) \
	  $(M)

.PHONY: test_build
test_build: init $(STACKTREE_ATDGEN_STUBS) $(INFERPRINT_ATDGEN_STUBS) $(CLANG_ATDGEN_STUBS) $(INFER_CLANG_FCP_MIRRORED_FILES)
	$(OCAMLBUILD_ALL) $(OCAMLBUILD_BYTE_OPTS) -build-dir $(TEST_BUILD_DIR) \
	  -cflags -warn-error,$(OCAML_FATAL_WARNINGS) \
	  $(INFER_ALL_TARGETS:.native=.byte)

# to generate interface file.mli from implementation file.ml execute:
#   make M=file mli
mli:
	ocamlfind ocamlc -package atdgen,oUnit,str,unix,yojson,zip $(addprefix -I ../_build/infer/,$(DEPENDENCIES)) -i $(M).ml > $(M).mli

rei:
	ocamlfind ocamlc -package atdgen,oUnit,str,unix,yojson,zip $(addprefix -I ../_build/infer/,$(DEPENDENCIES)) -i -pp refmt -impl $(M).re > $(M).rei

# convert to reason
%.re : %.ml
	refmt -assume-explicit-arity -heuristics-file unary.txt -parse ml -print re $< > $*.re

%.rei : %.mli
	refmt -assume-explicit-arity -heuristics-file unary.txt -parse ml -print re $< > $*.rei

roots:=Infer InferAnalyzeExe InferClang JMain InferPrintExe StatsAggregator
clusters:=base clang java IR

src_dirs:=$(shell find * -type d)
ml_src_files:=$(shell find $(src_dirs) -regex '.*\.ml\(i\)*' -not -path facebook/scripts/eradicate_stats.ml -not -path 'stubs/c/*')
re_src_files:=$(shell find $(src_dirs) -regex '.*\.re\(i\)*')
inc_flags:=$(foreach dir,$(src_dirs),-I $(dir))
root_flags:=$(foreach root,$(roots),-r $(root))
cluster_flags:=$(foreach cluster,$(clusters),-c $(cluster))

mod_dep.dot: $(ml_src_files) $(re_src_files)
	$(MAKE) -C $(DEPENDENCIES_DIR)/ocamldot
	{ ocamldep.opt $(inc_flags) -ml-synonym .re -mli-synonym .rei $(ml_src_files); \
	  ocamldep.opt $(inc_flags) -ml-synonym .re -mli-synonym .rei -pp refmt $(re_src_files); } \
	| $(DEPENDENCIES_DIR)/ocamldot/ocamldot $(cluster_flags) $(root_flags) \
	| grep -v -e "\"IList\"\|\"Utils\"" \
        > mod_dep.dot

mod_dep.pdf: mod_dep.dot
	dot -Tpdf -o mod_dep.pdf mod_dep.dot

.PHONY: dsort
dsort:
	@ocamldep.opt -sort $(inc_flags) -ml-synonym .re -mli-synonym .rei $(ml_src_files) -pp refmt $(re_src_files)

roots_grep_regex:=$(foreach root,$(roots),-e $(root)$$)
dirs_find_regex:=$(foreach dir, $(DEPENDENCIES) stubs/ml,-path "./$(dir)/*" -o)

.PHONY: toplevel
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
		-not \( -path "./unit/*" -o -path "./facebook/scripts/eradicate_stats.ml" -o -name ".#*" \) \
	| cut -c 3- \
	| rev | cut -f 2- -d '.' | rev \
	| awk 'BEGIN { FS = "/"; OFS = "/" } ; {$$NF=toupper(substr($$NF,1,1))substr($$NF,2); print $$0}' \
	| grep -v $(roots_grep_regex) > toplevel.mlpack
	$(OCAMLBUILD_ALL) -I stubs/ml -build-dir $(INFER_BUILD_DIR) toplevel.cmo

.PHONY: checkCopyright
checkCopyright: $(CHECKCOPYRIGHT_BIN)

$(CHECKCOPYRIGHT_BIN): sanitize $(BUILD_DIR) $(CHECKCOPYRIGHT_MAIN).ml
	rebuild -quiet -r -j $(NCPU) -build-dir $(INFER_BUILD_DIR) -pkgs core,str -tag thread -use-ocamlfind $(CHECKCOPYRIGHT_MAIN).native
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(CHECKCOPYRIGHT_MAIN).native $(CHECKCOPYRIGHT_BIN)

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
	$(INSTALL_DATA) -C $$< $$@
endef

$(foreach file, $(FCP_FILES_TO_MIRROR), $(eval $(call mirror_fcp_file,$(file))))


$(CLANG_BINIOU_DICT): $(CLANG_ATDGEN_STUB_ATD)
# overapproximation of the words we need in the biniou dictionary
# the long litany of symbols is [:punct:] minus "_-'"
	tr -s '[*!"#\$%&\(\)\+,\\\.\/:;<=>\?@\[\\\\]^`\{|\}~[:space:]]' '\n' \
	  < $< \
	  | sort | uniq  \
	  > $@

.PHONY: init
init: sanitize version $(BUILD_DIR)

.PHONY: sanitize
sanitize:
ifneq ($(wildcard $(BUILD_DIR)/sanitize.sh),)
	$(BUILD_DIR)/sanitize.sh
endif

.PHONY: version
version: base/Version.ml.in Makefile
	TMPFILE=$$(mktemp base/Version.ml.tmp.XXXX); \
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
	$(INSTALL_DATA) -C "$$TMPFILE" base/Version.ml; \
	rm -f "$$TMPFILE"

$(BUILD_DIR):
	$(MKDIR_P) $(BUILD_DIR)

.PHONY: test_clean
test_clean:
	$(REMOVE_DIR) $(TEST_BUILD_DIR)

.PHONY: clean
clean:
	$(REMOVE_DIR) $(BUILD_DIR)
	$(REMOVE) $(ETC_DIR)/clang_ast.dict
ifeq ($(ENABLE_OCAML_ANNOT),yes)
	$(REMOVE_DIR) $(ANNOT_DIR)
endif
	$(REMOVE) base/Version.ml
	$(REMOVE) base/Version.ml.tmp.*
	$(REMOVE) backend/jsonbug_{j,t}.ml{,i}
	$(REMOVE) checkers/stacktree_{j,t}.ml{,i}
	$(REMOVE) $(INFER_BIN) $(INFERANALYZE_BIN) $(INFERPRINT_BIN)
	$(REMOVE) $(INFERJAVA_BIN) $(INFERCLANG_BIN)
	$(REMOVE) $(INFERUNIT_BIN) $(CHECKCOPYRIGHT_BIN)
	$(REMOVE) $(CLANG_ATDGEN_STUBS)
	$(REMOVE) $(INFER_CLANG_FCP_MIRRORED_FILES)
	$(REMOVE) mod_dep.dot
	$(REMOVE) mod_dep.pdf
