# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

ROOT_DIR = ../..
include $(ROOT_DIR)/Makefile.config

#### Global declarations ####

ETC_DIR = $(INFER_DIR)/etc
# paths to BUILD_DIR are relative because that's how dune likes it
# can be overriden to specify another build mode (eg opt)
INFER_BUILD_DIR = _build/default

#### Backend declarations ####

INFER_MAIN = infer

#### Checkers declarations ####

INFER_ATDGEN_STUB_BASES = atd/jsonbug atd/runstate atd/java_method_decl atd/perf_profiler atd/java_profiler_samples atd/clang_profiler_samples
INFER_ATDGEN_TYPES = j t
INFER_ATDGEN_STUB_ATDS = $(INFER_ATDGEN_STUB_BASES:.atd)
INFER_ATDGEN_SUFFIXES = $(foreach atd_t,$(INFER_ATDGEN_TYPES),_$(atd_t).ml _$(atd_t).mli)
INFER_ATDGEN_STUBS = $(foreach base,$(INFER_ATDGEN_STUB_BASES),\
	$(addprefix $(base),$(INFER_ATDGEN_SUFFIXES)))

#### InferCreateTraceViewLinks declarations ####

FACEBOOK_DIR = facebook
INFER_CREATE_TRACEVIEW_LINKS_MODULE = InferCreateTraceViewLinks
INFER_CREATE_TRACEVIEW_LINKS_MAIN = $(INFER_CREATE_TRACEVIEW_LINKS_MODULE)

### InferUnit declarations ###

INFERUNIT_MAIN = inferunit

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

#### scripts declarations ####

CHECKCOPYRIGHT_BIN = $(SCRIPT_DIR)/checkCopyright
CHECKCOPYRIGHT_MAIN = checkCopyright

#### End of declarations ####

ifeq ($(ENABLE_OCAMLOPT_CUSTOM_CC),yes)
EXTRA_CFLAGS += -cc,$(CC)
endif

# list of ocamlbuild targets common to all build targets -- native version
INFER_CONFIG_TARGETS = $(INFER_BUILD_DIR)/$(INFER_MAIN).exe
ifeq ($(IS_FACEBOOK_TREE),yes)
INFER_CONFIG_TARGETS += $(INFER_BUILD_DIR)/$(INFER_CREATE_TRACEVIEW_LINKS_MAIN).exe
endif

OCAML_GENERATED_SOURCES = \
  base/Version.ml $(INFER_ATDGEN_STUBS)

ifeq ($(BUILD_C_ANALYZERS),yes)
OCAML_GENERATED_SOURCES += $(CLANG_ATDGEN_STUBS) $(CLANG_PLUGIN_MIRRORED_FILES)
endif

OCAML_SOURCES = \
  $(wildcard */[a-zA-Z]*.ml */[a-zA-Z]*.ml[ily] *.ml *.ml[ily]) \
  $(OCAML_GENERATED_SOURCES)

.PHONY: all
all: infer

GENERATED_FROM_AUTOCONF = dune.common dune-workspace base/Version.ml

GENERATED_DUNES += dune atd/dune base/dune IR/dune istd/dune scripts/dune

SRC_BUILD_COMMON = $(GENERATED_FROM_AUTOCONF) $(GENERATED_DUNES) $(OCAML_SOURCES)
ifeq ($(BUILD_C_ANALYZERS),yes)
SRC_BUILD_COMMON += $(CLANG_BINIOU_DICT)
endif

.PHONY: src_build_common
src_build_common: $(SRC_BUILD_COMMON)

# single out infer.exe as the source of truth for make, knowing that in fact several targets are
# produced by the build
$(INFER_BUILD_DIR)/$(INFER_MAIN).exe: $(SRC_BUILD_COMMON) $(MAKEFILE_LIST)
	$(QUIET)dune build $(INFER_CONFIG_TARGETS)
#	let make know that the target is up-to-date even if ocamlbuild cached it
	$(QUIET)touch $@

.PHONY: test
test: $(SRC_BUILD_COMMON) $(MAKEFILE_LIST)
	$(QUIET)dune build \
	  $(patsubst $(INFER_BUILD_DIR)/%.exe,_build/test/%.bc,$(INFER_CONFIG_TARGETS)) \
	  _build/test/scripts/checkCopyright.bc _build/test/$(INFERUNIT_MAIN).bc _build/test/infertop.bc

.PHONY: doc
doc: $(SRC_BUILD_COMMON) $(MAKEFILE_LIST)
	$(QUIET)dune build @$(INFER_BUILD_DIR)/doc

INFER_BIN_ALIASES = $(foreach alias,$(INFER_COMMANDS),$(BIN_DIR)/$(alias))

$(INFER_BIN_ALIASES): Makefile $(BIN_DIR)/$(INFER_MAIN)
	$(REMOVE) $@
	$(QUIET)cd $(@D) && $(LN_S) -f infer $(@F)
	$(QUIET)touch $@

$(INFER_BIN).exe: $(INFER_BUILD_DIR)/$(INFER_MAIN).exe
	$(QUIET)$(MKDIR_P) $(BIN_DIR)
ifeq ($(WINDOWS_BUILD),yes)
# http://cygwin.1069669.n5.nabble.com/cp-utility-bug-when-dest-name-exe-file-exist-tp57955p58001.html
	$(REMOVE) $(INFER_BIN) $(INFER_BIN).exe
endif
	$(INSTALL_PROGRAM) -C $(INFER_BUILD_DIR)/$(INFER_MAIN).exe $(INFER_BIN).exe
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(INSTALL_PROGRAM) -C $(INFER_BUILD_DIR)/$(INFER_CREATE_TRACEVIEW_LINKS_MAIN).exe \
		$(INFER_CREATE_TRACEVIEW_LINKS_BIN)
endif


.PHONY: infer
infer: $(INFER_BIN).exe
	$(INSTALL_PROGRAM) -C $(INFER_BIN).exe $(INFER_BIN)
	$(MAKE) $(INFER_BIN_ALIASES)

$(INFER_BUILD_DIR)/$(INFER_MAIN).bc: $(SRC_BUILD_COMMON) $(MAKEFILE_LIST)
	dune build $(INFER_CONFIG_TARGETS:.exe=.bc)
	$(QUIET)touch $@

$(INFER_BIN).bc: $(INFER_BUILD_DIR)/$(INFER_MAIN).bc
	$(QUIET)$(MKDIR_P) $(BIN_DIR)
ifeq ($(WINDOWS_BUILD),yes)
# http://cygwin.1069669.n5.nabble.com/cp-utility-bug-when-dest-name-exe-file-exist-tp57955p58001.html
	$(REMOVE) $(INFER_BIN) $(INFER_BIN).exe
endif
	$(INSTALL_PROGRAM) -C $(INFER_BUILD_DIR)/$(INFER_MAIN).bc $(INFER_BIN).bc
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(INSTALL_PROGRAM) -C $(INFER_BUILD_DIR)/$(INFER_CREATE_TRACEVIEW_LINKS_MAIN).bc \
		$(INFER_CREATE_TRACEVIEW_LINKS_BIN)
endif

.PHONY: byte
byte: $(INFER_BIN).bc
	$(INSTALL_PROGRAM) -C $(INFER_BIN).bc $(INFER_BIN)
	$(MAKE) $(INFER_BIN_ALIASES)

roots:=Infer
ifeq ($(IS_FACEBOOK_TREE),yes)
roots += $(INFER_CREATE_TRACEVIEW_LINKS_MODULE)
endif
clusters:=base clang java IR

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

$(INFER_BUILD_DIR)/infertop.bc: $(SRC_DIR)/infertop.ml $(SRC_BUILD_COMMON) $(MAKEFILE_LIST)
	dune build $@
	$(QUIET)touch $@

$(INFERTOP_BIN): $(INFER_BUILD_DIR)/infertop.bc
	$(QUIET)$(INSTALL_PROGRAM) -C $< $@
	$(QUIET)touch $@

.PHONY: toplevel
toplevel: $(INFERTOP_BIN)

.PHONY: checkCopyright
checkCopyright: $(CHECKCOPYRIGHT_BIN)

$(CHECKCOPYRIGHT_BIN): $(SRC_BUILD_COMMON) $(MAKEFILE_LIST)
	dune build $(INFER_BUILD_DIR)/scripts/$(CHECKCOPYRIGHT_MAIN).exe
	$(INSTALL_PROGRAM) -C $(INFER_BUILD_DIR)/scripts/$(CHECKCOPYRIGHT_MAIN).exe $(CHECKCOPYRIGHT_BIN)

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

$(foreach atd_type,$(INFER_ATDGEN_TYPES),\
  $(foreach base,$(INFER_ATDGEN_STUB_BASES),\
    $(eval \
        $(call gen_atdgen_rules,$(base).atd,$(base),$(atd_type)))))

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
dune-workspace: dune-workspace.in
base/Version.ml: base/Version.ml.in

$(GENERATED_DUNES): dune.common
	$(QUIET)cat $+ > $@

dune: dune.in
atd/dune: atd/dune.in
base/dune: base/dune.in
deadcode/dune: deadcode/dune.in
IR/dune: IR/dune.in
istd/dune: istd/dune.in
scripts/dune: scripts/dune.in

.PHONY: clean
clean:
	$(MAKE) -C deadcode clean
	$(REMOVE) $(INFER_TARGET)
	$(REMOVE) toplevel.mlpack
	$(REMOVE_DIR) $(BUILD_DIR)
	$(REMOVE) $(ETC_DIR)/clang_ast.dict
	$(REMOVE) $(GENERATED_FROM_AUTOCONF)
	$(REMOVE) $(GENERATED_DUNES)
	$(REMOVE) base/Version.ml.tmp.* dune.tmp.* dune-workspace.tmp.*
#	be a bit more aggressive than needed with what we remove here so that stale binaries that
#	only existed in previous versions get removed as well
	$(REMOVE) $(BIN_DIR)/Infer* $(BIN_DIR)/infer-* $(INFER_BIN){,.bc,.exe} $(INFER_BIN_ALIASES) \
	  $(INFERUNIT_BIN) $(CHECKCOPYRIGHT_BIN)
	$(REMOVE) $(BIN_DIR)/llvm_sil
	$(REMOVE) $(INFER_CREATE_TRACEVIEW_LINKS_BIN)
	$(REMOVE) atd/*_{j,t,v}.ml{,i} atd/clang_ast_*
	$(REMOVE) mod_dep.dot
	$(REMOVE) mod_dep.pdf

.PHONY: fmt
fmt:
	@$(MAKE) -C $(ROOT_DIR) fmt
