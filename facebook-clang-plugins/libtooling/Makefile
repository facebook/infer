# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.PHONY: clean all test all_ast_samples

LEVEL=..
include $(LEVEL)/Makefile.common

# ASTExporter
HEADERS+=atdlib/ATDWriter.h ASTExporter.h NamePrinter.h AttrParameterVectorStream.h SimplePluginASTAction.h FileUtils.h
OBJS+=ASTExporter.o SimplePluginASTAction.o FileUtils.o AttrParameterVectorStream.o

# Json
PLUGINS+=JsonASTExporter
EXTS+=.json

# Yojson
PLUGINS+=YojsonASTExporter
EXTS+=.yjson

# biniou
BINIOU_PLUGIN=BiniouASTExporter
PLUGINS+=$(BINIOU_PLUGIN)
EXTS+=.biniou

all: build/FacebookClangPlugin.dylib

include $(LEVEL)/Makefile.rules

CLANG_FRONTEND=$(CLANG) -fsyntax-only -Xpreprocessor -detailed-preprocessing-record -Xclang -load -Xclang $(shell pwd)/build/FacebookClangPlugin.dylib

build/FacebookClangPlugin.dylib: $(OBJS:%=build/%) $(HEADERS)
	@mkdir -p build
	$(CXX) $(LDFLAGS_DYLIB) -o $@ $(OBJS:%=build/%) -lz -lpthread -lm

TEST_DIRS=tests

OUT_TEST_FILES=${TEST_DIRS:%=%/*/*.out}

# To make sharing of test files easier, each source file should be
# found in 'tests'. A plugin will only use the source files for which
# a .exp file exists in the corresponding subdirectory.
EXPFILES_FORMULA=tests/$$P/*.exp
SRCFILE_FORMULA=tests/$$(basename $$TEST)
FILTERFILE_FORMULA=tests/$${P}/filter.sh

test: build/FacebookClangPlugin.dylib
	@for P in $(PLUGINS); do                                                        \
	   if [ "$$P" == "BiniouASTExporter" ] && ! hash bdump 2>/dev/null;             \
	   then continue;                                                               \
	   fi;                                                                          \
	   echo "-- $$P --";                                                            \
	   export CLANG_FRONTEND_PLUGIN__AST_WITH_POINTERS=0;                           \
	   rm -rf build/tmp_$$P;                                                        \
	   mkdir -p build/tmp_$$P;                                                      \
	   for EXPFILE in $(EXPFILES_FORMULA); do                                       \
	     TEST=$${EXPFILE%.exp};                                                     \
	     EXTRA_FLAGS="";                                                            \
	     case "$(SRCFILE_FORMULA)" in                                               \
	     *.c )                                                                      \
	       EXTRA_FLAGS="-target x86_64-unknown-linux";                              \
	       ;;                                                                       \
	     *.m )                                                                      \
	       if [ "$(HAS_OBJC)" = "no" ]; then                                        \
	         printf "[~] %s skipped (no Objective-C support)\n"                     \
	           "$$(basename $(SRCFILE_FORMULA))";                                   \
	         continue;                                                              \
	       fi;                                                                      \
	       EXTRA_FLAGS="-ObjC -fblocks $(IOSFLAGS)";                                \
	       ;;                                                                       \
	     *.cpp )                                                                    \
	       EXTRA_FLAGS="--std=c++14 -target x86_64-unknown-linux";                  \
	       ;;                                                                       \
	     *.mm )                                                                     \
	       EXTRA_FLAGS="--std=c++14 -ObjC++ -fblocks $(IOSFLAGS)";                  \
	       ;;                                                                       \
	     esac;                                                                      \
	     $(RUNTEST) "$$TEST" $(FILTERFILE_FORMULA)                                  \
	       $(CLANG_FRONTEND) $$EXTRA_FLAGS -Xclang -plugin -Xclang $$P              \
	       -Xclang -plugin-arg-$$P -Xclang -                                        \
	       -Xclang -plugin-arg-$$P -Xclang USE_TEMP_DIR_FOR_DEDUPLICATION=build/tmp_$$P \
	       -c $(SRCFILE_FORMULA);                                                   \
	   done;                                                                        \
	done
	@if [ ! $$KEEP_TEST_OUTPUTS ]; then rm -f $(OUT_TEST_FILES); fi

record-test-outputs:
	@$(MAKE) DEBUG=1 KEEP_TEST_OUTPUTS=1 test || true
	@for F in $(OUT_TEST_FILES); do cp $$F $${F%.out}.exp; done
	@rm -f $(OUT_TEST_FILES)

clean:
	@rm -rf build/* $(OUT_TEST_FILES)

# -- AST samples for specific end-to-end tests --

REGULAR_SOURCES=$(wildcard tests/*.m) $(wildcard tests/*.c) $(wildcard tests/*.cpp) $(wildcard tests/*.mm)
AST_SAMPLE_FILES=ASTExporter.cpp $(REGULAR_SOURCES:tests/%=%)
all_ast_samples: $(AST_SAMPLE_FILES:%=build/ast_samples/%.json.gz) $(AST_SAMPLE_FILES:%=build/ast_samples/%.yjson.gz)

# dump samples files in Yojson using ASTExporter.cpp
YJ_DUMPER_ARGS=-Xclang -plugin -Xclang YojsonASTExporter -Xclang -plugin-arg-YojsonASTExporter -Xclang

build/ast_samples/%.cpp.yjson: %.cpp build/FacebookClangPlugin.dylib
	@mkdir -p build/ast_samples
	$(CLANG_FRONTEND) $(CFLAGS) -Wno-ignored-qualifiers -I. $(YJ_DUMPER_ARGS) $@ -c $<

build/ast_samples/%.cpp.yjson: tests/%.cpp build/FacebookClangPlugin.dylib
	@mkdir -p build/ast_samples
	$(CLANG_FRONTEND) --std=c++14 $(YJ_DUMPER_ARGS) $@ -c $<

build/ast_samples/%.c.yjson: tests/%.c build/FacebookClangPlugin.dylib
	@mkdir -p build/ast_samples
	$(CLANG_FRONTEND) $(YJ_DUMPER_ARGS) $@ -c $<

build/ast_samples/%.m.yjson: tests/%.m build/FacebookClangPlugin.dylib
	@mkdir -p build/ast_samples
	$(CLANG_FRONTEND) $(IOSFLAGS) $(YJ_DUMPER_ARGS) $@ -c $<

build/ast_samples/%.mm.yjson: tests/%.mm build/FacebookClangPlugin.dylib
	@mkdir -p build/ast_samples
	$(CLANG_FRONTEND) --std=c++14 $(IOSFLAGS) $(YJ_DUMPER_ARGS) $@ -c $<

# dump sample files in Yojson using ASTExporter.cpp
J_DUMPER_ARGS=-Xclang -plugin -Xclang JsonASTExporter -Xclang -plugin-arg-JsonASTExporter -Xclang

build/ast_samples/%.cpp.json: %.cpp build/FacebookClangPlugin.dylib
	@mkdir -p build/ast_samples
	$(CLANG_FRONTEND) $(CFLAGS) -Wno-ignored-qualifiers -I. $(J_DUMPER_ARGS) $@ -c $<

build/ast_samples/%.cpp.json: tests/%.cpp build/FacebookClangPlugin.dylib
	@mkdir -p build/ast_samples
	$(CLANG_FRONTEND) --std=c++14 $(J_DUMPER_ARGS) $@ -c $<

build/ast_samples/%.c.json: tests/%.c build/FacebookClangPlugin.dylib
	@mkdir -p build/ast_samples
	$(CLANG_FRONTEND) $(J_DUMPER_ARGS) $@ -c $<

build/ast_samples/%.m.json: tests/%.m build/FacebookClangPlugin.dylib
	@mkdir -p build/ast_samples
	$(CLANG_FRONTEND) $(IOSFLAGS) $(J_DUMPER_ARGS) $@ -c $<

build/ast_samples/%.gz: build/ast_samples/%
	@gzip -f < $< > $@

# generate a preprocessed version of ASTExporter.cpp where ATD directives can be read
# we do not include the C/C++ headers to avoid parsing issues with the 'traditional' cpp (needed for precisely expanding our /// comments)
build/ASTExporter.h.p: ASTExporter.h
	@mkdir -p build
	@cat $< | grep -v '#include *["<][^.]*\(\.h\)\?[">]' | $(ATD_CPP) -I$(CLANG_PREFIX)/include > $@
