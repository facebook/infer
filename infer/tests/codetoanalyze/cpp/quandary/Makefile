# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

include ../../Makefile.clang

OPTIONS = -x c++ -std=c++11 -isystem$(ROOT_DIR) -isystem$(CLANG_INCLUDES)/c++/v1/ -c

ANALYZER = quandary
INFERPRINT_OPTIONS = --issues-txt

FILES = \
  basics.cpp \
  execs.cpp \

compile:
	clang $(OPTIONS) $(FILES)

analyze:
	$(call silent_on_success,$(INFER_BIN) -a $(ANALYZER) --cxx --ml-buckets cpp --check-duplicate-symbols -- clang $(OPTIONS) $(FILES) 2>duplicates.txt)
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0
