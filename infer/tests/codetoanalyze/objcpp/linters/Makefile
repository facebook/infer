# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

include ../../Makefile.clang

OPTIONS = -x objective-c++ -std=c++11 -fobjc-arc -c

ANALYZER = linters

FILES = \
  */*.mm \

compile:
	clang $(OPTIONS) $(FILES)

analyze:
	$(call silent_on_success,$(INFER_BIN) -a $(ANALYZER) --check-duplicate-symbols -- clang $(OPTIONS) $(FILES) 2>duplicates.txt)
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0
