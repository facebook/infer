# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.PHONY: all clean test

LEVEL=..
include $(LEVEL)/Makefile.common

LIBTOOLING=$(LEVEL)/libtooling
ATDLIB=$(LEVEL)/libtooling/atdlib

# see README for the required versions
ATDGEN=atdgen
ATDCAT=atdcat -i

OCAMLOPT=ocamlfind ocamlopt -package unix,atdgen,atdgen-runtime,camlzip -I build -safe-string

all: build/clang_ast_converter build/clang_ast_named_decl_printer

# type definitions
%_t.ml %_t.mli: %.atd
	$(ATDGEN) -t -o $* $<

# yojson
%_j.ml %_j.mli: %.atd
	$(ATDGEN) -j -o $* $<

# biniou
%_b.ml %_b.mli: %.atd
	$(ATDGEN) -b -o $* $<

# validator
%_v.ml %_v.mli: %.atd
	$(ATDGEN) -v -o $* $<

# standard ocaml rules
build/%.cmx:%.ml
	$(OCAMLOPT) -o $@ -c $<

build/%.cmi:%.mli
	$(OCAMLOPT) -o $@ -c $<

build/%.cmx:build/%.ml
	$(OCAMLOPT) -o $@ -c $<

build/%.cmi:build/%.mli
	$(OCAMLOPT) -o $@ -c $<

# clang AST
build/ast_inline.atd.p: $(LIBTOOLING)/ASTExporter.h
	@mkdir -p build
	@$(MAKE) -C $(LIBTOOLING) build/ASTExporter.h.p
	$(ATDLIB)/extract_atd_from_cpp.py $(LIBTOOLING)/build/ASTExporter.h.p | $(ATDLIB)/normalize_names_in_atd.py > $@

build/ast_inline.atd: build/ast_inline.atd.p
	$(ATD_CPP) $< > $@

build/clang_ast.atd: build/ast_inline.atd
#	$(ATDCAT) returns 0 even if it fails - look at the output file to
#	determine whether it failed or not. Then remove the result file as
#	make mysteriously doesn't do that by itself
	$(ATDCAT) $< > $@ && { [ -s $@ ] || { rm $@; exit 1; }; }

build/ast_inline.atd.inc: build/ast_inline.atd.p
	cat $< | grep '^#' > $@

build/ast_inline.pat.inc: build/ast_inline.atd.inc
	cat $< | sed 's/*/,/g; s/ list/_list/g' > $@

build/clang_ast_proj.mli: clang_ast_proj.mli.p build/ast_inline.atd.inc
	cat $< | $(ATD_CPP) -include build/ast_inline.atd.inc > $@

build/clang_ast_proj.ml: clang_ast_proj.ml.p build/ast_inline.pat.inc
	$(OCAML_CPP) -I$(CLANG_PREFIX)/include $< -o - | $(ATDLIB)/normalize_names_in_atd.py | $(ATD_CPP) -include build/ast_inline.pat.inc > $@

# test files except for Objective-C
TEST_FILES_COMMON=c_attributes.c bind_temporary.cpp \
c_cast.cpp const_cast.cpp dynamic_cast.cpp expr_with_cleanups.cpp inheritance.cpp \
lambda.cpp materialize_temporary.cpp namespace_decl.cpp new.cpp struct.cpp this.cpp \
unresolved_lookup.cpp using_directive.cpp class_template.cpp function_template.cpp \
type_trait.cpp no_except_expr.cpp friend.cpp

TEST_FILES=$(TEST_FILES_COMMON)

# only add Objective-C(++) files on mac
ifeq ($(HAS_OBJC),yes)
TEST_FILES+=Hello.m ObjCTest.m ObjCBridgeTransferTest.m objcpp_template_unboxing.mm
endif

PRINTER_TEST_FILES=$(TEST_FILES)
CONVERTER_TEST_FILE=struct.cpp
YOJSON_VALIDATOR_TEST_FILE=$(TEST_FILES)

# simple library for composing unix processes
build/process_test: build/process.cmx build/process_test.cmx
	$(OCAMLOPT) -linkpkg -o $@ $^

# Utilities
build/utils_test: build/process.cmx build/utils.cmx build/utils_test.cmx
	$(OCAMLOPT) -linkpkg -o $@ $^

CLANG_AST_LIBS=$(patsubst %,build/%.cmx,clang_ast_types clang_ast_t clang_ast_j process utils yojson_utils)

build/yojson_utils_test: $(CLANG_AST_LIBS) build/yojson_utils_test.cmx
	$(OCAMLOPT) -linkpkg -o $@ $^

build/clang_ast_proj_test: $(CLANG_AST_LIBS) build/clang_ast_proj.cmx build/clang_ast_proj_test.cmx
	$(OCAMLOPT) -linkpkg -o $@ $^

# AST format converter
build/clang_ast_converter: $(CLANG_AST_LIBS) build/clang_ast_converter.cmx
	$(OCAMLOPT) -linkpkg -o $@ $^

CLANG_AST_PROJ_LIBS=$(patsubst %,build/%.cmx,clang_ast_types clang_ast_t clang_ast_j clang_ast_proj clang_ast_visit clang_ast_v clang_ast_main)

# example of AST visitor
build/clang_ast_named_decl_printer: $(CLANG_AST_PROJ_LIBS) build/clang_ast_named_decl_printer.cmx
	$(OCAMLOPT) -linkpkg -o $@ $^

build/clang_ast_main_test: $(CLANG_AST_PROJ_LIBS) build/clang_ast_main_test.cmx
	$(OCAMLOPT) -linkpkg -o $@ $^

test: $(patsubst %,build/%,process_test utils_test yojson_utils_test clang_ast_proj_test clang_ast_converter clang_ast_named_decl_printer clang_ast_main_test)
	@$(MAKE) -C $(LIBTOOLING) $(PRINTER_TEST_FILES:%=build/ast_samples/%.yjson) $(TEST_FILES:%=build/ast_samples/%.yjson.gz) $(CONVERTER_TEST_FILE:%=build/ast_samples/%.yjson)
	@export LIMIT=100; \
	 $(RUNTEST) tests/process_test build/process_test; \
	 $(RUNTEST) tests/utils_test build/utils_test; \
	 $(RUNTEST) tests/yojson_utils_test build/yojson_utils_test; \
	 $(RUNTEST) tests/clang_ast_proj_test build/clang_ast_proj_test; \
	 if [ "$(HAS_OBJC)" = "yes" ]; then \
	   $(RUNTEST) tests/clang_ast_named_decl_printer build/clang_ast_named_decl_printer $(PRINTER_TEST_FILES:%=$(LIBTOOLING)/build/ast_samples/%.yjson); \
	 else \
	   printf "[~] %s skipped (no Objective-C support)\n" clang_ast_named_decl_printer; \
	 fi; \
	 $(RUNTEST) tests/clang_ast_converter build/clang_ast_converter --pretty $(CONVERTER_TEST_FILE:%=$(LIBTOOLING)/build/ast_samples/%.yjson); \
	 $(RUNTEST) tests/clang_ast_yojson_validation ./yojson_validator.sh build/clang_ast_converter $(YOJSON_VALIDATOR_TEST_FILE:%=$(LIBTOOLING)/build/ast_samples/%.yjson.gz); \
	 if [ "$(HAS_OBJC)" = "yes" ]; then \
	   $(RUNTEST) tests/clang_ast_main_test build/clang_ast_main_test $(PRINTER_TEST_FILES:%=$(LIBTOOLING)/build/ast_samples/%.yjson); \
	 else \
	   printf "[~] %s skipped (no Objective-C support)\n" clang_ast_main_test; \
	 fi
	@if [ ! $$KEEP_TEST_OUTPUTS ]; then rm -f tests/*.out; fi

record-test-outputs:
	@rm -f tests/*.out
	@$(MAKE) DEBUG=1 KEEP_TEST_OUTPUTS=1 test || true
	@for F in tests/*.out; do cp $$F $${F%.out}.exp; done
	@rm -f tests/*.out

.PHONY: fmt
fmt:
	ocamlformat -i $(wildcard *.ml) $(wildcard *.mli)

-include .depend

.depend: $(wildcard *.ml) $(wildcard *.mli) build/clang_ast_t.mli build/clang_ast_t.ml build/clang_ast_j.mli build/clang_ast_j.ml build/clang_ast_v.mli build/clang_ast_v.ml build/clang_ast_proj.mli build/clang_ast_proj.ml
	ocamldep -native -I build $^ | sed -e 's/\([a-zA-Z0-9_]*\.cm.\)/build\/\1/g' | sed -e 's/build\/build\//build\//g' > .depend

clean:
	rm -rf tests/*.out build .depend
