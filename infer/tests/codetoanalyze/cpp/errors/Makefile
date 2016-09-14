# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

include ../../Makefile.clang

OPTIONS = -x c++ -std=c++11 -isystem$(CLANG_INCLUDES)/c++/v1/ -c

FILES = \
  attributes/*.cpp \
  conditional/*.cpp \
  constructors/*.cpp \
  c_tests/*.cpp \
  exceptions/*.cpp \
  include_header/header.h \
  include_header/include_templ.cpp \
  lambda/*.cpp \
  memory_leaks/*.cpp \
  methods/*.cpp \
  models/*.cpp \
  namespace/*.cpp \
  nestedoperators/*.cpp \
  npe/*.cpp \
  numeric/*.cpp \
  overwrite_attribute/*.cpp \
  reference/*.cpp \
  resource_leaks/*.cpp \
  smart_ptr/*.cpp \
  subtyping/*.cpp \
  templates/*.cpp \
  types/*.cpp \
  vector/*.cpp \

compile:
	clang $(OPTIONS) $(FILES)

analyze:
	$(INFER_BIN) -a $(ANALYZER) --cxx --ml-buckets cpp --check-duplicate-symbols -- clang $(OPTIONS) $(FILES) >/dev/null 2>duplicates.txt
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0
