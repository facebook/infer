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
ifeq ($(TEST),1)
BASE_BUILD_DIR = $(BUILD_DIR)/test
else
BASE_BUILD_DIR = $(BUILD_DIR)
endif
INFER_BUILD_DIR = $(BASE_BUILD_DIR)/infer

ATDGEN_SUFFIXES = _t.ml _t.mli _j.ml _j.mli

#### ocamlbuild options ####

GENERATED_OCAML_SOURCES_GLOB = <*{clang_plugin/*,backend/jsonbug_*,checkers/stacktree_*}>

OCAML_FATAL_WARNINGS = +3+5+6+8+10+11+12+18+19+20+21+23+26+29+27+33+34+35+37+38+39+50+52+57-50

# options for ocamlbuild
# Notice that use-ocamlfind is used here to avoid a linker bug in ocamlbuild when using -tag thread
OCAMLBUILD_OPTIONS = \
  -r \
  -use-menhir -menhir '$(MENHIR) --explain --strict'\
  -use-ocamlfind \
  -cflags -g -lflags -g \
  -cflags -short-paths \
  -cflags -safe-string \
  -cflags -principal \
  -cflags -strict-formats \
  -cflags -strict-sequence \
  -cflags -w,$(OCAML_FATAL_WARNINGS)-4-9-32-40-41-42-45-48 \
  -tag-line "$(GENERATED_OCAML_SOURCES_GLOB): warn(-27-32-34-35-39)" \
  -tag-line "not $(GENERATED_OCAML_SOURCES_GLOB) and <*/{,*/}*.{ml,re}{,i}>: package(ppx_compare)" \
  -tag-line "not $(GENERATED_OCAML_SOURCES_GLOB) and not <*{base/IList,base/IStd}.*>: open(IStd)" \
  -tag thread \
  -pkgs ANSITerminal,atdgen,cmdliner,core,extlib,oUnit,parmap,str,unix,xmlm,yojson,zip

ifeq ($(ENABLE_OCAML_BINANNOT),yes)
OCAMLBUILD_OPTIONS += -cflags -bin-annot
endif

ifeq ($(ENABLE_OCAMLOPT_CUSTOM_CC),yes)
OCAMLBUILD_OPTIONS += -lflags -cc,$(CC) -cflags -cc,$(CC)
endif

ifneq (,$(findstring s,$(MAKEFLAGS)))
OCAMLBUILD_OPTIONS += -quiet
endif

ifeq ($(TEST),1)
OCAMLBUILD_OPTIONS += -cflags -warn-error,$(OCAML_FATAL_WARNINGS)
endif

export OCAMLFIND_IGNORE_DUPS_IN=$(shell $(OCAMLC) -where)/compiler-libs

#### Backend declarations ####

INFER_MAIN = backend/infer

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
INFER_CREATE_TRACEVIEW_LINKS_MAIN = $(FACEBOOK_DIR)/$(INFER_CREATE_TRACEVIEW_LINKS_MODULE)

### InferUnit declarations ###

UNIT_SOURCES = unit

INFERUNIT_MAIN = $(UNIT_SOURCES)/inferunit

#### Java declarations ####

JAVA_OCAMLBUILD_OPTIONS = -pkgs javalib,ptrees,sawja

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
CHECKCOPYRIGHT_MAIN = $(SCRIPT_SOURCES)/checkCopyright

#### End of declarations ####

ifeq ($(IS_FACEBOOK_TREE),yes)
EXTRA_DEPS = facebook
else
EXTRA_DEPS = opensource
endif

DEPENDENCIES = \
  absint backend base bufferoverrun checkers eradicate harness integration IR labs quandary unit \
  $(EXTRA_DEPS)

# ocamlbuild command with options common to all build targets
OCAMLBUILD_BASE = \
  $(OCAMLBUILD) $(OCAMLBUILD_OPTIONS) -j $(NCPU) $(addprefix -I , $(DEPENDENCIES)) \

# ocamlbuild with options necessary to build all targets at once, regardless of configure flags
OCAMLBUILD_ALL = $(OCAMLBUILD_BASE) $(JAVA_OCAMLBUILD_OPTIONS)

# configure-aware ocamlbuild commands and targets
OCAMLBUILD_CONFIG = $(OCAMLBUILD_BASE)
# list of ocamlbuild targets common to all build targets -- native version
INFER_CONFIG_TARGETS = $(INFER_MAIN).native
ifeq ($(IS_FACEBOOK_TREE),yes)
INFER_CONFIG_TARGETS += $(INFER_CREATE_TRACEVIEW_LINKS_MAIN).native
endif

ifeq ($(BUILD_JAVA_ANALYZERS),yes)
OCAMLBUILD_CONFIG += $(JAVA_OCAMLBUILD_OPTIONS)
DEPENDENCIES += java
else
DEPENDENCIES += java_stubs
endif
ifeq ($(BUILD_C_ANALYZERS),yes)
DEPENDENCIES += clang clang_plugin unit/clang
else
DEPENDENCIES += clang_stubs unit/clang_stubs
endif
ifeq ($(TEST),1)
INFER_CONFIG_TARGETS += $(INFERUNIT_MAIN).native
endif

OCAML_BASE_SOURCES = \
  $(wildcard $(DEPENDENCIES:=/[a-zA-Z]*.ml) $(DEPENDENCIES:=/[a-zA-Z]*.mli) \
             $(DEPENDENCIES:=/[a-zA-Z]*.mll) $(DEPENDENCIES:=/[a-zA-Z]*.mly) \
             $(DEPENDENCIES:=/[a-zA-Z]*.re) $(DEPENDENCIES:=/[a-zA-Z]*.rei)) \
  $(STACKTREE_ATDGEN_STUBS) $(INFERPRINT_ATDGEN_STUBS)
OCAML_CONFIG_SOURCES = $(OCAML_BASE_SOURCES)
ifeq ($(BUILD_C_ANALYZERS),yes)
OCAML_CONFIG_SOURCES += $(CLANG_ATDGEN_STUBS) $(CLANG_PLUGIN_MIRRORED_FILES)
endif
OCAML_ALL_SOURCES = $(OCAML_BASE_SOURCES) $(CLANG_ATDGEN_STUBS) $(CLANG_PLUGIN_MIRRORED_FILES)

.PHONY: all
all: infer

# single out infer.native as the source of truth for make, knowing that in fact several targets are
# produced by the build
$(INFER_BUILD_DIR)/$(INFER_MAIN).native: base/Version.ml $(OCAML_CONFIG_SOURCES) \
                                         $(MAKEFILE_LIST)
	$(MKDIR_P) $(BASE_BUILD_DIR)
	$(OCAMLBUILD_CONFIG) -build-dir $(INFER_BUILD_DIR) \
	  $(INFER_CONFIG_TARGETS)
#	let make know that the target is up-to-date even if ocamlbuild cached it
	$(QUIET)touch $@

INFER_BIN_ALIASES = $(foreach alias,$(INFER_COMMANDS),$(BIN_DIR)/$(alias))

$(INFER_BIN_ALIASES): Makefile
#	make sure the bin directory exists
	$(QUIET)$(MKDIR_P) $(@D)
	$(QUIET)cd $(@D) && $(LN_S) -f infer $(@F)
	$(QUIET)touch $@

$(INFER_BIN).native: $(INFER_BUILD_DIR)/$(INFER_MAIN).native $(INFER_BIN_ALIASES)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFER_MAIN).native $(INFER_BIN)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFER_MAIN).native $(INFER_BIN).native
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFER_CREATE_TRACEVIEW_LINKS_MAIN).native \
		$(INFER_CREATE_TRACEVIEW_LINKS_BIN)
endif


.PHONY: infer
infer: $(INFER_BIN).native

ifeq ($(BUILD_C_ANALYZERS),yes)
infer: $(CLANG_BINIOU_DICT)
endif

$(INFER_BUILD_DIR)/$(INFER_MAIN).byte: base/Version.ml $(OCAML_CONFIG_SOURCES) \
                                       $(MAKEFILE_LIST)
	$(MKDIR_P) $(BASE_BUILD_DIR)
	$(OCAMLBUILD_CONFIG) -build-dir $(INFER_BUILD_DIR) \
	  $(INFER_CONFIG_TARGETS:.native=.byte)
	$(QUIET)touch $@

$(INFER_BIN).byte: $(INFER_BUILD_DIR)/$(INFER_MAIN).byte $(INFER_BIN_ALIASES)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFER_MAIN).byte $(INFER_BIN)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFER_MAIN).byte $(INFER_BIN).byte
ifeq ($(TEST),1)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFERUNIT_MAIN).byte $(INFERUNIT_BIN)
endif
ifeq ($(IS_FACEBOOK_TREE),yes)
	$(INSTALL_PROGRAM) $(INFER_BUILD_DIR)/$(INFER_CREATE_TRACEVIEW_LINKS_MAIN).byte \
		$(INFER_CREATE_TRACEVIEW_LINKS_BIN)
endif

.PHONY: byte
byte: $(INFER_BIN).byte

.PHONY: byte_no_install
byte_no_install: $(INFER_BUILD_DIR)/$(INFER_MAIN).byte

# to build only the single module <Module> (and its dependencies) with extra flags execute:
#   make MFLAGS=<flags> M=<Module>.cm{o,x} module
# for example, to build the assembly for the Ident module, execute
#   make MFLAGS="-ocamlopt 'ocamlopt -S'" M=Ident.cmx module
M=
MFLAGS=

.PHONY: module
module: base/Version.ml $(OCAML_ALL_SOURCES)
	$(MKDIR_P) $(BASE_BUILD_DIR)
	$(OCAMLBUILD_ALL) -build-dir $(INFER_BUILD_DIR) \
          $(MFLAGS) \
	  $(M)

# to generate interface file.mli from implementation file.ml execute:
#   make M=file mli
mli:
	$(OCAMLFIND) ocamlc -package atdgen,oUnit,str,unix,yojson,zip $(addprefix -I $(INFER_BUILD_DIR),$(DEPENDENCIES)) -i $(M).ml > $(M).mli

roots:=Infer
ifeq ($(IS_FACEBOOK_TREE),yes)
roots += $(INFER_CREATE_TRACEVIEW_LINKS_MODULE)
endif
clusters:=base clang java IR

ml_src_files:=$(shell find $(DEPENDENCIES) -regex '.*\.ml\(i\)*')
inc_flags:=$(foreach dir,$(DEPENDENCIES),-I $(dir))
root_flags:=$(foreach root,$(roots),-r $(root))
cluster_flags:=$(foreach cluster,$(clusters),-c $(cluster))

mod_dep.dot: $(ml_src_files)
	$(MAKE) -C $(DEPENDENCIES_DIR)/ocamldot
	ocamldep.opt $(inc_flags) $(ml_src_files) \
	| $(DEPENDENCIES_DIR)/ocamldot/ocamldot $(cluster_flags) $(root_flags) \
	| grep -v -e "\"IList\"\|\"Utils\"" \
        > mod_dep.dot

mod_dep.pdf: mod_dep.dot
	dot -Tpdf -o mod_dep.pdf mod_dep.dot

.PHONY: dsort
dsort:
	$(QUIET)ocamldep.opt -sort $(inc_flags) $(ml_src_files)

define to_ocaml_module
$(shell \
  echo $(basename $(1)) \
  | awk 'BEGIN { FS = "/"; OFS = "/" } ; {$$NF=toupper(substr($$NF,1,1))substr($$NF,2); print $$0}')
endef

toplevel.mlpack: base/Version.ml $(OCAML_CONFIG_SOURCES) $(MAKEFILE_LIST)
#	We need to pack all the infer modules into another module to avoid name clashes with some
#	of them coming from ocaml libraries (Ident for example).  To do that, we generate a .mlpack
#	file containing namespaced modules.
#	1. we filter out roots because they execute code upon loading
#	2. each source file is converted to a module by capitalizing its first letter
#	3. we `echo` each module individually to avoid too long a command line
#	4. prevent race conditions by using a temp file
	$(eval $@_tmp := $(shell mktemp $@.tmp.XXXX))
	$(foreach module,\
	  $(filter-out $(foreach root,$(roots),%/$(root)),\
	  $(foreach source,\
	    $(filter-out unit/%,$(OCAML_CONFIG_SOURCES)),\
	    $(call to_ocaml_module,$(source)))),\
	  $(shell echo $(module) >> $($@_tmp)))
	mv $($@_tmp) $@

$(INFER_BUILD_DIR)/toplevel.cmo: toplevel.mlpack
	$(MKDIR_P) $(BASE_BUILD_DIR)
	$(OCAMLBUILD_CONFIG) -build-dir $(INFER_BUILD_DIR) toplevel.cmo
	$(QUIET)touch $@

.PHONY: toplevel
toplevel: $(INFER_BUILD_DIR)/toplevel.cmo

.PHONY: checkCopyright
checkCopyright: $(CHECKCOPYRIGHT_BIN)

$(CHECKCOPYRIGHT_BIN): $(CHECKCOPYRIGHT_MAIN).ml $(MAKEFILE_LIST)
	$(MKDIR_P) $(BASE_BUILD_DIR)
	$(OCAMLBUILD) -quiet -r -j $(NCPU) -build-dir $(BASE_BUILD_DIR)/checkCopyright \
	  -cflags -g,-safe-string -lflags -g \
	  -pkgs core,str -tag thread -use-ocamlfind $(CHECKCOPYRIGHT_MAIN).native
	$(INSTALL_PROGRAM) \
	  $(BASE_BUILD_DIR)/checkCopyright/$(CHECKCOPYRIGHT_MAIN).native $(CHECKCOPYRIGHT_BIN)

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

base/Version.ml: base/Version.ml.in $(MAKEFILE_LIST)
	TMPFILE=$$(mktemp $@.tmp.XXXX); \
	INFER_GIT_COMMIT=$$(git --work-tree=$(ROOT_DIR) --git-dir=$(ROOT_DIR)/.git rev-parse --short HEAD || printf "unknown"); \
	INFER_GIT_BRANCH=$$(git --work-tree=$(ROOT_DIR) --git-dir=$(ROOT_DIR)/.git rev-parse --abbrev-ref HEAD || printf "unknown"); \
	sed \
	  -e 's|@INFER_MAJOR[@]|$(INFER_MAJOR)|g' \
	  -e 's|@INFER_MINOR[@]|$(INFER_MINOR)|g' \
	  -e 's|@INFER_PATCH[@]|$(INFER_PATCH)|g' \
	  -e 's|@IS_RELEASE_TREE[@]|$(IS_RELEASE_TREE)|g' \
	  -e "s|@INFER_GIT_COMMIT[@]|$$INFER_GIT_COMMIT|g" \
	  -e "s|@INFER_GIT_BRANCH[@]|$$INFER_GIT_BRANCH|g" \
	  -e "s|@BUILD_C_ANALYZERS[@]|$(BUILD_C_ANALYZERS)|g" \
	  -e "s|@BUILD_JAVA_ANALYZERS[@]|$(BUILD_JAVA_ANALYZERS)|g" \
	  -e "s|@XCODE_SELECT[@]|$(XCODE_SELECT)|g" \
	  -e "s|@INFER_MAN_LAST_MODIFIED[@]|$(INFER_MAN_LAST_MODIFIED)|g" \
	  $< > "$$TMPFILE"; \
	cat "$$TMPFILE" > $@; \
	$(REMOVE) "$$TMPFILE"

.PHONY: clean
clean:
	$(REMOVE) $(INFER_TARGET)
	$(REMOVE) toplevel.mlpack
	$(REMOVE_DIR) $(BUILD_DIR)
	$(REMOVE) $(ETC_DIR)/clang_ast.dict
	$(REMOVE) base/Version.ml
	$(REMOVE) base/Version.ml.tmp.*
	$(REMOVE) backend/jsonbug_{j,t}.ml{,i}
	$(REMOVE) checkers/stacktree_{j,t}.ml{,i}
#	be a bit more aggressive than needed with what we remove here so that stale binaries that
#	only existed in previous versions get removed as well
	$(REMOVE) $(BIN_DIR)/Infer* $(BIN_DIR)/infer-* $(INFER_BIN){,.byte,.native} $(INFER_BIN_ALIASES) \
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
