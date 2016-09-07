# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

include ../../Makefile.clang

OPTIONS = -c

FILES = \
  */*.c \

compile:
	clang $(OPTIONS) $(FILES)

analyze:
	INFER_REPORT_CUSTOM_ERROR=1 \
	$(INFER_BIN) -a $(ANALYZER) --check-duplicate-symbols --developer-mode -- clang $(OPTIONS) $(FILES) >/dev/null 2>duplicates.txt
	grep "DUPLICATE_SYMBOLS" duplicates.txt; test $$? -ne 0
