# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = $(TESTS_DIR)/../..

OBJECTS = $(foreach source,$(SOURCES),$(basename $(source)).o)
CLEAN_EXTRA += duplicates.txt

include $(TESTS_DIR)/base.make

infer-out/report.json: $(CLANG_DEPS) $(SOURCES) $(HEADERS)
	$(call silent_on_success,\
	  $(INFER_BIN) --check-duplicate-symbols $(INFER_OPTIONS) -a $(ANALYZER) -- clang $(CLANG_OPTIONS) $(SOURCES) 2>duplicates.txt)
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0

%.o: %.c
	clang $(CLANG_OPTIONS) -o $@ $<

%.o: %.cpp
	clang++ $(CLANG_OPTIONS) -o $@ $<

%.o: %.m
	clang $(CLANG_OPTIONS) -o $@ $<

%.o: %.mm
	clang++ $(CLANG_OPTIONS) -o $@ $<
