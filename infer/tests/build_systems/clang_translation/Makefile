# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..

# see explanations in cpp/errors/Makefile for the custom isystem
CLANG_OPTIONS = -x c++ -std=c++11 -nostdinc++ -isystem$(CLANG_INCLUDES)/c++/v1/ -c

ROOT = ../codetoanalyze/clang_translation/src
SYM_ROOT = ../codetoanalyze/clang_translation/tsrc_symlink

SOURCES = \
  $(ROOT)/main.cpp \
  $(ROOT)/main_default_root.cpp \
  $(SYM_ROOT)/main_symlink.cpp \
  $(SYM_ROOT)/main_default_symlink.cpp \

DOT_FILES = $(SOURCES:=.test.dot)
CLEAN_EXTRA = $(DOT_FILES) $(foreach source,$(SOURCES),infer-out-$(basename $(notdir $(source))))

include $(TESTS_DIR)/clang-frontend.make

INFER_OPTIONS = -a capture --frontend-tests --skip-translation-headers exclude_dir --no-failures-allowed

capture: $(DOT_FILES)

$(ROOT)/main.cpp.test.dot $(SYM_ROOT)/main_symlink.cpp.test.dot: $(SOURCES) $(CLANG_DEPS)
	$(call silent_on_success,\
	$(INFER_BIN) $(INFER_OPTIONS) --results-dir infer-out-$(basename $(@F:.test.dot=)) \
	  --project-root $(ROOT) --icfg-dotty-outfile $@ -- \
	  clang $(CLANG_OPTIONS) $(@:.test.dot=))

$(ROOT)/main_default_root.cpp.test.dot $(SYM_ROOT)/main_default_symlink.cpp.test.dot: \
  $(CLANG_DEPS) $(SOURCES)
	cd $(@D) && \
	$(call silent_on_success,\
	$(INFER_BIN) $(INFER_OPTIONS) --results-dir $(CURDIR)/infer-out-$(basename $(@F:.test.dot=)) \
	  --icfg-dotty-outfile $(@F) -- \
	  clang $(CLANG_OPTIONS) $(@F:.test.dot=))

# test_extra needs to be separate target. Otherwise commands from test
# target in common Makefile won't run
test: test_extra

# all dot files should be exactly the same - if they aren't there is something wrong
test_extra: $(DOT_FILES)
	diff -u $(ROOT)/main.cpp.test.dot $(SYM_ROOT)/main_symlink.cpp.test.dot
	diff -u $(ROOT)/main.cpp.test.dot $(ROOT)/main_default_root.cpp.test.dot
	diff -u $(ROOT)/main.cpp.test.dot $(SYM_ROOT)/main_default_symlink.cpp.test.dot
