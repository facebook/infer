# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

ROOT_DIR = ../..
include $(ROOT_DIR)/Makefile.config

#### Global declarations ####

INFER_MAIN = infer
INFERTOP_MAIN = infertop
INFERUNIT_MAIN = inferunit
INFER_CREATE_TRACEVIEW_LINKS_MODULE = InferCreateTraceViewLinks
INFER_CREATE_TRACEVIEW_LINKS_MAIN = $(INFER_CREATE_TRACEVIEW_LINKS_MODULE)
CHECKCOPYRIGHT_MAIN = checkCopyright

#### Clang declarations ####

CLANG_PLUGIN_MIRROR = atd

FCP_CLANG_OCAML_BUILD_DIR = $(FCP_CLANG_OCAML_DIR)/build

CLANG_PLUGIN_BINARIES = $(addprefix $(FCP_CLANG_OCAML_BUILD_DIR), \
                          clang_ast_converter clang_ast_named_decl_printer)

CLANG_AST_BASE_NAME = clang_ast
CLANG_ATDGEN_STUB_BASE = $(CLANG_PLUGIN_MIRROR)/$(CLANG_AST_BASE_NAME)
CLANG_ATDGEN_STUB_ATD = $(FCP_CLANG_OCAML_BUILD_DIR)/$(CLANG_AST_BASE_NAME).atd
CLANG_ATDGEN_TYPES = b j t v
CLANG_ATDGEN_SUFFIXES = $(foreach atd_t,$(CLANG_ATDGEN_TYPES),_$(atd_t).ml _$(atd_t).mli)
CLANG_ATDGEN_STUBS = $(addprefix $(CLANG_ATDGEN_STUB_BASE), $(CLANG_ATDGEN_SUFFIXES))

FCP_CLANG_AST_PROJ = $(addprefix $(FCP_CLANG_OCAML_BUILD_DIR)/, \
                       clang_ast_proj.ml clang_ast_proj.mli)
FCP_CLANG_AST_MAIN = $(addprefix $(FCP_CLANG_OCAML_DIR)/, clang_ast_visit.ml clang_ast_types.ml)
FCP_FILES_TO_MIRROR = $(FCP_CLANG_AST_PROJ) $(FCP_CLANG_AST_MAIN)
CLANG_PLUGIN_MIRRORED_FILES = $(addprefix $(CLANG_PLUGIN_MIRROR)/, $(notdir $(FCP_FILES_TO_MIRROR)))

CLANG_BINIOU_DICT = $(ETC_DIR)/clang_ast.dict

ifeq ($(ENABLE_OCAMLOPT_CUSTOM_CC),yes)
EXTRA_CFLAGS += -cc,$(CC)
endif

# list of ocamlbuild targets common to all build targets -- native version
INFER_CONFIG_TARGETS = $(INFER_MAIN).exe
ifeq ($(IS_FACEBOOK_TREE),yes)
INFER_CONFIG_TARGETS += $(INFER_CREATE_TRACEVIEW_LINKS_MAIN).exe
endif

OCAML_GENERATED_SOURCES = base/Version.ml

ifeq ($(BUILD_C_ANALYZERS),yes)
OCAML_GENERATED_SOURCES += $(CLANG_ATDGEN_STUBS) $(CLANG_PLUGIN_MIRRORED_FILES)
endif

OCAML_SOURCES = \
  $(wildcard */[a-zA-Z]*.ml */[a-zA-Z]*.ml[ily] *.ml *.ml[ily]) \
  $(OCAML_GENERATED_SOURCES)

.PHONY: all
all: infer

GENERATED_FROM_AUTOCONF = dune.common ../dune-workspace base/Version.ml

.PHONY: dune-workspace
dune-workspace: ../dune-workspace

GENERATED_DUNES += dune al/dune clang/dune integration/dune java/dune opensource/dune unit/dune

SRC_BUILD_COMMON = $(GENERATED_FROM_AUTOCONF) $(GENERATED_DUNES) $(OCAML_SOURCES)
ifeq ($(BUILD_C_ANALYZERS),yes)
SRC_BUILD_COMMON += $(CLANG_BINIOU_DICT)
endif

.PHONY: src_build_common
src_build_common: $(SRC_BUILD_COMMON)

# NOTE: the complexity below is because @runtest and @fmt interact poorly: we want dune to format
# test files *after* having promoted them when the test outputs change
.PHONY: runtest
runtest: $(SRC_BUILD_COMMON) $(MAKEFILE_LIST)
	$(QUIET)exit_code=0; \
	$(DUNE_BUILD) @runtest --auto-promote || exit_code=$$?; \
	$(DUNE_BUILD) @fmt --auto-promote || exit_code=$$?; \
	exit $$exit_code

.PHONY: test
test: $(SRC_BUILD_COMMON) $(MAKEFILE_LIST) runtest
	$(DUNE_BUILD) \
	  $(patsubst %.exe, %.bc.exe, $(INFER_CONFIG_TARGETS)) \
	  scripts/$(CHECKCOPYRIGHT_MAIN).bc.exe $(INFERUNIT_MAIN).bc.exe $(INFERTOP_MAIN).bc.exe

.PHONY: unit
unit: runtest test
	$(QUIET)$(REMOVE_DIR) infer-out-unit-tests
	$(QUIET)INFER_ARGS=--results-dir^infer-out-unit-tests $(INFERUNIT_BIN)

.PHONY: build_all
build_all: $(SRC_BUILD_COMMON)
	$(DUNE_BUILD) @all

.PHONY: doc
doc: $(SRC_BUILD_COMMON) $(MAKEFILE_LIST)
	$(QUIET)cd .. && $(DUNE_BUILD) @doc

.PHONY: check
check: src_build_common
	$(DUNE_BUILD) @check

#### Aliases for building binaries ####

# Single out infer[.bc].exe as the source of truth for make, knowing that in fact several
# targets are produced by the build.
# The targets below are marked as .PHONY because if you build with one profile `make
# BUILD_MODE=dev` and then try with another profile `make BUILD_MODE=opt`, make won't
# trigger a rebuild since it has only partial view on the dependencies.

.PHONY: $(INFER_BIN).exe
$(INFER_BIN).exe:  $(SRC_BUILD_COMMON) $(MAKEFILE_LIST)
	$(QUIET)$(DUNE_BUILD) $(INFER_CONFIG_TARGETS)

.PHONY: $(INFER_BIN).bc.exe
$(INFER_BIN).bc.exe:  $(SRC_BUILD_COMMON) $(MAKEFILE_LIST)
	$(QUIET)$(DUNE_BUILD) $(patsubst %.exe, %.bc.exe, $(INFER_CONFIG_TARGETS))

.PHONY: $(INFERTOP_BIN) toplevel
$(INFERTOP_BIN): $(SRC_BUILD_COMMON) $(MAKEFILE_LIST)
	$(DUNE_BUILD) $(INFERTOP_MAIN).bc.exe
toplevel: $(INFERTOP_BIN)

.PHONY: $(CHECKCOPYRIGHT_BIN) checkCopyright
$(CHECKCOPYRIGHT_BIN): $(SRC_BUILD_COMMON) $(MAKEFILE_LIST)
	$(DUNE_BUILD) scripts/$(CHECKCOPYRIGHT_MAIN)
checkCopyright: $(CHECKCOPYRIGHT_BIN)

.PHONY: infer
infer: $(INFER_BIN).exe
	$(INSTALL_PROGRAM) -C $(INFER_BIN).exe $(INFER_BIN)
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(INSTALL_PROGRAM) -C $(INFER_CREATE_TRACEVIEW_LINKS_BIN).exe $(INFER_CREATE_TRACEVIEW_LINKS_BIN)
endif
	$(MAKE) $(INFER_BIN_ALIASES)

.PHONY: byte
byte: $(INFER_BIN).bc.exe
	$(INSTALL_PROGRAM) -C $(INFER_BIN).bc.exe $(INFER_BIN)
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(INSTALL_PROGRAM) -C $(INFER_CREATE_TRACEVIEW_LINKS_BIN).bc.exe $(INFER_CREATE_TRACEVIEW_LINKS_BIN)
endif
	$(MAKE) $(INFER_BIN_ALIASES)

INFER_BIN_ALIASES = $(foreach alias,$(INFER_COMMANDS),$(BIN_DIR)/$(alias))
$(INFER_BIN_ALIASES): Makefile $(BIN_DIR)/$(INFER_MAIN)
	$(REMOVE) $@
	$(QUIET)cd $(@D) && $(LN_S) -f infer $(@F)
	$(QUIET)touch $@

roots:=Infer
ifeq ($(IS_FACEBOOK_TREE),yes)
roots += $(INFER_CREATE_TRACEVIEW_LINKS_MODULE)
endif
clusters:=absint al atd backend base biabduction bufferoverrun checkers clang clang_stubs concurrency cost c_stubs deadcode integration IR istd java labs dotnet llvm nullsafe opensource pulse quandary scripts test_determinator topl unit

ml_src_files:=$(shell find . -not -path "./*stubs*" -regex '\./[a-zA-Z].*\.ml\(i\)*')
inc_flags:=$(foreach dir,$(shell find . -not -path './_build*' -type d),-I $(dir))
root_flags:=$(foreach root,$(roots),-r $(root))
cluster_flags:=$(foreach cluster,$(clusters),-c $(cluster))

mod_dep.dot: $(ml_src_files)
	$(MAKE) -C $(DEPENDENCIES_DIR)/ocamldot
	ocamldep.opt $(inc_flags) $(ml_src_files) \
	| $(DEPENDENCIES_DIR)/ocamldot/ocamldot $(cluster_flags) $(root_flags) \
	| grep -v -e "\"IList\"\|\"Utils\"\|\"IStd\"\|\"Infertop\"" \
        > mod_dep.dot

mod_dep.pdf: mod_dep.dot
	dot -Tpdf -o mod_dep.pdf mod_dep.dot

.PHONY: dsort
dsort:
	$(QUIET)ocamldep.opt -sort $(inc_flags) $(ml_src_files)

define gen_atdgen_rules
# generate files using atdgen
# parameters:
#   1. the .atd file to generate .ml{,i} files from, e.g. foo.atd
#   2. the base name of .ml{,i} files, e.g. foo
#   3. the type of files to generate: b, j, t, or v

$(2)_$(3).mli: $(1)
	$(ATDGEN) -$(3) $$< -o $(2)

# the .ml depends on the corresponding .mli to avoid running atdgen
# twice during parallel builds
$(2)_$(3).ml: $(2)_$(3).mli
endef

# rebuild the artifacts of the AST files whenever they're upated in FCP
$(foreach atd_type,$(CLANG_ATDGEN_TYPES),\
    $(eval \
        $(call gen_atdgen_rules,$(CLANG_ATDGEN_STUB_ATD),$(CLANG_ATDGEN_STUB_BASE),$(atd_type))))


define mirror_fcp_file
$(CLANG_PLUGIN_MIRROR)/$(notdir $(1)): $(1)
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

$(GENERATED_FROM_AUTOCONF): $(MAKEFILE_LIST)
	TMPFILE=$$(mktemp $@.tmp.XXXX); \
	INFER_GIT_COMMIT=$$(git --work-tree=$(ROOT_DIR) --git-dir=$(ROOT_DIR)/.git rev-parse --short HEAD || printf "unknown"); \
	INFER_GIT_BRANCH=$$(git --work-tree=$(ROOT_DIR) --git-dir=$(ROOT_DIR)/.git rev-parse --abbrev-ref HEAD || printf "unknown"); \
	sed \
	  -e 's|@EXTRA_CFLAGS[@]|$(EXTRA_CFLAGS)|g' \
	  -e 's|@INFER_MAJOR[@]|$(INFER_MAJOR)|g' \
	  -e 's|@INFER_MINOR[@]|$(INFER_MINOR)|g' \
	  -e 's|@INFER_PATCH[@]|$(INFER_PATCH)|g' \
	  -e 's|@IS_FACEBOOK_TREE[@]|$(IS_FACEBOOK_TREE)|g' \
	  -e 's|@IS_RELEASE_TREE[@]|$(IS_RELEASE_TREE)|g' \
	  -e "s|@INFER_GIT_COMMIT[@]|$$INFER_GIT_COMMIT|g" \
	  -e "s|@INFER_GIT_BRANCH[@]|$$INFER_GIT_BRANCH|g" \
	  -e "s|@JAVA_MAJOR_VERSION[@]|$(JAVA_MAJOR_VERSION)|g" \
	  -e "s|@BUILD_C_ANALYZERS[@]|$(BUILD_C_ANALYZERS)|g" \
	  -e "s|@BUILD_JAVA_ANALYZERS[@]|$(BUILD_JAVA_ANALYZERS)|g" \
	  -e "s|@BUILD_PLATFORM[@]|$(BUILD_PLATFORM)|g" \
	  -e "s|@OPAMSWITCH[@]|$(OPAMSWITCH)|g" \
	  -e "s|@XCODE_SELECT[@]|$(XCODE_SELECT)|g" \
	  -e "s|@INFER_MAN_LAST_MODIFIED[@]|$(INFER_MAN_LAST_MODIFIED)|g" \
	  $@.in > "$$TMPFILE"; \
	cat "$$TMPFILE" > $@; \
	$(REMOVE) "$$TMPFILE"
dune.common: dune.common.in
../dune-workspace: ../dune-workspace.in
base/Version.ml: base/Version.ml.in

$(GENERATED_DUNES): dune.common
	$(QUIET)cat $+ > $@

dune: dune.in
al/dune: al/dune.in
clang/dune: clang/dune.in
integration/dune: integration/dune.in
java/dune: java/dune.in
opensource/dune: opensource/dune.in
unit/dune: unit/dune.in

.PHONY: clean
clean:
	$(MAKE) -C deadcode clean
	$(REMOVE) $(INFER_TARGET)
	$(REMOVE) toplevel.mlpack
	$(REMOVE) $(ETC_DIR)/clang_ast.dict
	$(REMOVE) $(GENERATED_FROM_AUTOCONF)
	$(REMOVE) $(GENERATED_DUNES)
	$(REMOVE) base/Version.ml.tmp.* dune.tmp.* dune-workspace.tmp.*
#	be a bit more aggressive than needed with what we remove here so that stale binaries that
#	only existed in previous versions get removed as well
	$(REMOVE) $(INFER_BIN) $(INFER_BIN_ALIASES) $(INFER_CREATE_TRACEVIEW_LINKS_BIN)
	$(REMOVE) $(BIN_DIR)/llvm_sil
	$(REMOVE) atd/*_{j,t,v}.ml{,i} atd/clang_ast_*
	$(REMOVE) mod_dep.dot
	$(REMOVE) mod_dep.pdf
	cd .. && dune clean

.PHONY: fmt
fmt:
	@$(MAKE) -C $(ROOT_DIR) fmt
