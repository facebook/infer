# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

# see explanations in cpp/pulse/Makefile for the custom isystem
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

INFER_OPTIONS = capture --frontend-tests --skip-translation-headers exclude_dir

capture: $(DOT_FILES)

$(ROOT)/main.cpp.test.dot $(SYM_ROOT)/main_symlink.cpp.test.dot: $(SOURCES) $(CLANG_DEPS)
	$(QUIET)$(call silent_on_success,Testing clang translation with symlinks with project root,\
	$(INFER_BIN) $(INFER_OPTIONS) --results-dir infer-out-$(basename $(@F:.test.dot=)) \
	  --project-root $(ROOT) --icfg-dotty-outfile $@ -- \
	  clang $(CLANG_OPTIONS) $(@:.test.dot=))

$(ROOT)/main_default_root.cpp.test.dot $(SYM_ROOT)/main_default_symlink.cpp.test.dot: \
  $(CLANG_DEPS) $(SOURCES)
	$(QUIET)cd $(@D) && \
	$(call silent_on_success,Testing clang translation with symlinks with CWD=project root,\
	$(INFER_BIN) $(INFER_OPTIONS) --results-dir $(CURDIR)/infer-out-$(basename $(@F:.test.dot=)) \
	  --icfg-dotty-outfile $(@F) -- \
	  clang $(CLANG_OPTIONS) $(@F:.test.dot=))

# test_extra needs to be separate target. Otherwise commands from test
# target in common Makefile won't run
test: test_extra

# all dot files should be exactly the same - if they aren't there is something wrong
test_extra: $(DOT_FILES)
	$(QUIET)$(call check_no_diff,$(ROOT)/main.cpp.test.dot,\
	                         $(SYM_ROOT)/main_symlink.cpp.test.dot)
	$(QUIET)$(call check_no_diff,$(ROOT)/main.cpp.test.dot,\
	                             $(ROOT)/main_default_root.cpp.test.dot)
	$(QUIET)$(call check_no_diff,$(ROOT)/main.cpp.test.dot,\
	                         $(SYM_ROOT)/main_default_symlink.cpp.test.dot)
