# Copyright (c) 2013 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = ../..
include $(ROOT_DIR)/Makefile.config

#### Global declarations ####

ETC_DIR = $(INFER_DIR)/etc
# paths to BUILD_DIR are relative because that's how jbuilder likes it
JBUILDER_BUILD_DEFAULT = _build/default
JBUILDER_BUILD_TEST = _build/test
ifeq ($(TEST),1)
INFER_BUILD_DIR = $(JBUILDER_BUILD_TEST)
else
INFER_BUILD_DIR = $(JBUILDER_BUILD_DEFAULT)
endif

ATDGEN_SUFFIXES = _t.ml _t.mli _j.ml _j.mli

#### Backend declarations ####

INFER_MAIN = infer

#### Checkers declarations ####

STACKTREE_ATDGEN_STUB_BASE = checkers/stacktree
STACKTREE_ATDGEN_STUB_ATD = $(STACKTREE_ATDGEN_STUB_BASE).atd
STACKTREE_ATDGEN_STUBS = $(addprefix $(STACKTREE_ATDGEN_STUB_BASE), $(ATDGEN_SUFFIXES))

#### InferPrint declarations ####

INFERPRINT_ATDGEN_STUB_BASE = backend/jsonbug
INFERPRINT_ATDGEN_STUB_ATD = $(INFERPRINT_ATDGEN_STUB_BASE).atd
INFERPRINT_ATDGEN_STUBS = $(addprefix $(INFERPRINT_ATDGEN_STUB_BASE), $(ATDGEN_SUFFIXES))

#### InferCreateTraceViewLinks declarations ####

FACEBOOK_DIR = facebook
INFER_CREATE_TRACEVIEW_LINKS_MODULE = InferCreateTraceViewLinks
INFER_CREATE_TRACEVIEW_LINKS_MAIN = $(INFER_CREATE_TRACEVIEW_LINKS_MODULE)

### InferUnit declarations ###

UNIT_SOURCES = unit

INFERUNIT_MAIN = inferunit

#### Java declarations ####

JAVA_SOURCES = java

#### Clang declarations ####

CLANG_SOURCES = clang

CLANG_PLUGIN_MIRROR = clang_plugin

FCP_CLANG_OCAML_BUILD_DIR = $(FCP_CLANG_OCAML_DIR)/build

CLANG_PLUGIN_BINARIES = $(addprefix $(FCP_CLANG_OCAML_BUILD_DIR), \
                          clang_ast_converter clang_ast_named_decl_printer)

CLANG_AST_BASE_NAME = clang_ast
CLANG_ATDGEN_STUB_BASE = $(CLANG_PLUGIN_MIRROR)/$(CLANG_AST_BASE_NAME)
CLANG_ATDGEN_STUB_ATD = $(FCP_CLANG_OCAML_BUILD_DIR)/$(CLANG_AST_BASE_NAME).atd
CLANG_ATDGEN_SUFFIXES = _t.ml _t.mli _b.ml _b.mli _j.ml _j.mli _v.ml _v.mli
CLANG_ATDGEN_STUBS = $(addprefix $(CLANG_ATDGEN_STUB_BASE), $(CLANG_ATDGEN_SUFFIXES))

FCP_CLANG_AST_PROJ = $(addprefix $(FCP_CLANG_OCAML_BUILD_DIR)/, \
                       clang_ast_proj.ml clang_ast_proj.mli)
FCP_CLANG_AST_MAIN = $(addprefix $(FCP_CLANG_OCAML_DIR)/, clang_ast_visit.ml clang_ast_types.ml)
FCP_FILES_TO_MIRROR = $(FCP_CLANG_AST_PROJ) $(FCP_CLANG_AST_MAIN)
CLANG_PLUGIN_MIRRORED_FILES = $(addprefix $(CLANG_PLUGIN_MIRROR)/, $(notdir $(FCP_FILES_TO_MIRROR)))

CLANG_BINIOU_DICT = $(ETC_DIR)/clang_ast.dict

#### scripts declarations ####

SCRIPT_SOURCES = scripts

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

ifeq ($(TEST),1)
INFER_CONFIG_TARGETS += $(INFER_BUILD_DIR)/$(INFERUNIT_MAIN).exe
endif

OCAML_SOURCES = \
  $(wildcard */[a-zA-Z]*.ml */[a-zA-Z]*.ml[ily]) \
  base/Version.ml $(STACKTREE_ATDGEN_STUBS) $(INFERPRINT_ATDGEN_STUBS)

ifeq ($(BUILD_C_ANALYZERS),yes)
OCAML_SOURCES += $(CLANG_ATDGEN_STUBS) $(CLANG_PLUGIN_MIRRORED_FILES)
endif

.PHONY: all
all: infer

$(INFER_BUILD_DIR)/.ppx/ppx_compare/ppx.exe: jbuild jbuild-workspace $(OCAML_SOURCES)
#      some voodoo to make jbuilder tolerate being run in parallel: force jbuilder to build its
#      jbuild files and some files that have been seen to race otherwise
	jbuilder build \
	  $(JBUILDER_BUILD_DEFAULT)/.ppx/ppx_compare/ppx.exe \
	  $(JBUILDER_BUILD_TEST)/.ppx/ppx_compare/ppx.exe
	touch $@

SRC_BUILD_COMMON = $(INFER_BUILD_DIR)/.ppx/ppx_compare/ppx.exe $(OCAML_SOURCES)
ifeq ($(BUILD_C_ANALYZERS),yes)
SRC_BUILD_COMMON += $(CLANG_BINIOU_DICT)
endif

.PHONY: src_build_common
src_build_common: $(SRC_BUILD_COMMON)

# single out infer.exe as the source of truth for make, knowing that in fact several targets are
# produced by the build
$(INFER_BUILD_DIR)/$(INFER_MAIN).exe: $(SRC_BUILD_COMMON) $(MAKEFILE_LIST)
	jbuilder build $(INFER_CONFIG_TARGETS)
#	let make know that the target is up-to-date even if ocamlbuild cached it
	$(QUIET)touch $@

INFER_BIN_ALIASES = $(foreach alias,$(INFER_COMMANDS),$(BIN_DIR)/$(alias))

$(INFER_BIN_ALIASES): Makefile
#	make sure the bin directory exists
	$(QUIET)$(MKDIR_P) $(@D)
	$(QUIET)cd $(@D) && $(LN_S) -f infer $(@F)
	$(QUIET)touch $@

$(INFER_BIN).exe: $(INFER_BUILD_DIR)/$(INFER_MAIN).exe $(INFER_BIN_ALIASES)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFER_MAIN).exe $(INFER_BIN)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFER_MAIN).exe $(INFER_BIN).exe
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFER_CREATE_TRACEVIEW_LINKS_MAIN).exe \
		$(INFER_CREATE_TRACEVIEW_LINKS_BIN)
endif


.PHONY: infer
infer: $(INFER_BIN).exe

$(INFER_BUILD_DIR)/$(INFER_MAIN).bc: $(SRC_BUILD_COMMON) $(MAKEFILE_LIST)
	jbuilder build $(INFER_CONFIG_TARGETS:.exe=.bc)
	$(QUIET)touch $@

$(INFER_BIN).bc: $(INFER_BUILD_DIR)/$(INFER_MAIN).bc $(INFER_BIN_ALIASES)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFER_MAIN).bc $(INFER_BIN)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFER_MAIN).bc $(INFER_BIN).bc
ifeq ($(TEST),1)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFERUNIT_MAIN).bc $(INFERUNIT_BIN)
endif
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFER_CREATE_TRACEVIEW_LINKS_MAIN).bc \
		$(INFER_CREATE_TRACEVIEW_LINKS_BIN)
endif

.PHONY: byte
byte: $(INFER_BIN).bc

.PHONY: byte_no_install
byte_no_install: $(INFER_BUILD_DIR)/$(INFER_MAIN).bc

roots:=Infer
ifeq ($(IS_FACEBOOK_TREE),yes)
roots += $(INFER_CREATE_TRACEVIEW_LINKS_MODULE)
endif
clusters:=base clang java IR

ml_src_files:=$(shell find . -regex '.*\.ml\(i\)*')
inc_flags:=$(foreach dir,$(shell find . -type d),-I $(dir))
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
	jbuilder build $@
	$(QUIET)touch $@

.PHONY: toplevel
toplevel: $(INFER_BUILD_DIR)/infertop.bc

.PHONY: checkCopyright
checkCopyright: $(CHECKCOPYRIGHT_BIN)

$(CHECKCOPYRIGHT_BIN): scripts/$(CHECKCOPYRIGHT_MAIN).ml $(MAKEFILE_LIST)
	jbuilder build $(INFER_BUILD_DIR)/scripts/$(CHECKCOPYRIGHT_MAIN).exe
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/scripts/$(CHECKCOPYRIGHT_MAIN).exe $(CHECKCOPYRIGHT_BIN)

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
$(CLANG_PLUGIN_MIRROR)/$(notdir $(1)): $(1)
	$(INSTALL_DATA) $$< $$@
endef

$(foreach file, $(FCP_FILES_TO_MIRROR), $(eval $(call mirror_fcp_file,$(file))))


$(CLANG_BINIOU_DICT): $(CLANG_ATDGEN_STUB_ATD)
# overapproximation of the words we need in the biniou dictionary
# the long litany of symbols is [:punct:] minus "_-'"
	tr -s '[*!"#\$%&\(\)\+,\\\.\/:;<=>\?@\[\\\\]^`\{|\}~[:space:]]' '\n' \
	  < $< \
	  | sort | uniq  \
	  > $@

jbuild jbuild-workspace base/Version.ml: $(MAKEFILE_LIST)
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
	  -e "s|@BUILD_C_ANALYZERS[@]|$(BUILD_C_ANALYZERS)|g" \
	  -e "s|@BUILD_JAVA_ANALYZERS[@]|$(BUILD_JAVA_ANALYZERS)|g" \
	  -e "s|@OPAMSWITCH[@]|$(OPAMSWITCH)|g" \
	  -e "s|@XCODE_SELECT[@]|$(XCODE_SELECT)|g" \
	  -e "s|@INFER_MAN_LAST_MODIFIED[@]|$(INFER_MAN_LAST_MODIFIED)|g" \
	  $@.in > "$$TMPFILE"; \
	cat "$$TMPFILE" > $@; \
	$(REMOVE) "$$TMPFILE"
jbuild: jbuild.in
jbuild-workspace: jbuild-workspace.in
base/Version.ml: base/Version.ml.in

.PHONY: clean
clean:
	$(REMOVE) $(INFER_TARGET)
	$(REMOVE) toplevel.mlpack
	$(REMOVE_DIR) $(BUILD_DIR)
	$(REMOVE) $(ETC_DIR)/clang_ast.dict
	$(REMOVE) base/Version.ml jbuild jbuild-workspace
	$(REMOVE) base/Version.ml.tmp.* jbuild.tmp.* jbuild-workspace.tmp.*
	$(REMOVE) backend/jsonbug_{j,t}.ml{,i}
	$(REMOVE) checkers/stacktree_{j,t}.ml{,i}
#	be a bit more aggressive than needed with what we remove here so that stale binaries that
#	only existed in previous versions get removed as well
	$(REMOVE) $(BIN_DIR)/Infer* $(BIN_DIR)/infer-* $(INFER_BIN){,.bc,.exe} $(INFER_BIN_ALIASES) \
	  $(INFERUNIT_BIN) $(CHECKCOPYRIGHT_BIN)
	$(REMOVE) $(INFER_CREATE_TRACEVIEW_LINKS_BIN)
	$(REMOVE) $(CLANG_PLUGIN_MIRROR)/*
	$(REMOVE) mod_dep.dot
	$(REMOVE) mod_dep.pdf

.PHONY: fmt
fmt:
	@$(MAKE) -C $(ROOT_DIR) fmt

# print any variable for Makefile debugging
print-%:
	$(QUIET)echo '$*=$($*)'
