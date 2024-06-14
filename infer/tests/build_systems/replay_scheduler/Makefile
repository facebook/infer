# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

SRC = $(wildcard *.c)

include $(TESTS_DIR)/base.make

schedule.exp.test: $(INFER_BIN) $(SRC) $(MAKEFILE_LIST)
	$(QUIET)$(COPY) schedule.exp $@
	$(QUIET)$(INFER_BIN) --pulse-only --debug-exceptions --no-pulse-force-continue \
	  --project-root $(TESTS_DIR) \
	  --replay-analysis-schedule --analysis-schedule-file $@ --store-analysis-schedule \
	  -- clang -c $(SRC)

.PHONY: test
test: schedule.exp.test
	$(QUIET)diff -u schedule.exp $<

.PHONY: replace
replace: schedule.exp.test
	$(QUIET)$(COPY) $< schedule.exp

.PHONY: clean
clean:
	$(QUIET)$(REMOVE_DIR) infer-out schedule.exp.test $(SRC:.c=.o)
