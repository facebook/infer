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

include $(TESTS_DIR)/clang-frontend.make

INFER_OPTIONS = -a capture --frontend-tests --cxx --skip-translation-headers exclude_dir

compile:
	clang $(CLANG_OPTIONS) $(SOURCES)

capture:
	$(INFER_BIN) $(INFER_OPTIONS) --project-root $(ROOT) \
	--icfg-dotty-outfile $(ROOT)/main.cpp.test.dot -- \
	clang $(CLANG_OPTIONS) $(ROOT)/main.cpp

	$(INFER_BIN) $(INFER_OPTIONS) --project-root $(SYM_ROOT) \
	--icfg-dotty-outfile $(SYM_ROOT)/main_symlink.cpp.test.dot -- \
	clang $(CLANG_OPTIONS) $(SYM_ROOT)/main.cpp

	cd $(ROOT) && $(INFER_BIN) $(INFER_OPTIONS) \
	--icfg-dotty-outfile main_default_root.cpp.test.dot -- \
	clang $(CLANG_OPTIONS) main.cpp

	cd $(SYM_ROOT) && $(INFER_BIN) $(INFER_OPTIONS) \
	--icfg-dotty-outfile main_default_symlink.cpp.test.dot -- \
	clang $(CLANG_OPTIONS) main.cpp

# test_extra needs to be separate target. Otherwise commands from test
# target in common Makefile won't run
test: test_extra

# all dot files should be exactly the same - if they aren't there is something wrong
test_extra:
	diff $(ROOT)/main.cpp.dot $(SYM_ROOT)/main_symlink.cpp.dot && \
	diff $(ROOT)/main.cpp.dot $(ROOT)/main_default_root.cpp.dot && \
	diff $(ROOT)/main.cpp.dot $(SYM_ROOT)/main_default_symlink.cpp.dot
