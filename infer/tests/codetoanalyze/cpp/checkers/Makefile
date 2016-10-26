# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

include ../../Makefile.clang

OPTIONS = -x c++ -std=c++11 -isystem$(MODELS_DIR)/cpp/include -isystem$(CLANG_INCLUDES)/c++/v1/ -c

ANALYZER = checkers
INFERPRINT_OPTIONS = --issues-txt

FILES = \
  siof/const.cpp \
  siof/const_use.cpp \
  siof/pod_across_translation_units-1.cpp \
  siof/pod_across_translation_units-2.cpp \
  siof/pod_same_translation_unit.cpp \
  siof/siof_across_translation_units-1.cpp \
  siof/siof_across_translation_units-2.cpp \

compile:
	clang $(OPTIONS) $(FILES)

analyze:
	$(INFER_BIN) -a $(ANALYZER) --cxx --ml-buckets cpp --check-duplicate-symbols -- clang $(OPTIONS) $(FILES) >/dev/null 2>duplicates.txt
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0
