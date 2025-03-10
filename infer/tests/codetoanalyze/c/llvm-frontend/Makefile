# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR=../../..


INFER_OPTIONS =

SOURCES = $(wildcard *.c)

CLEAN_EXTRA = $(foreach source,$(SOURCES),$(basename $(notdir $(source))).o)

include $(TESTS_DIR)/base.make
include $(TESTS_DIR)/clang-base.make

default: test

%.c.test.sil: %.c $(LLAIR_DEPS) $(HEADERS)
	$(QUIET)$(call silent_on_success,Testing the infer/llvm frontend in $(TEST_REL_DIR),\
	  $(INFER_BIN) capture --frontend-tests --project-root $(TESTS_DIR) $(INFER_OPTIONS) --capture-llair $*.llair --llair-source-file $<)

capture: $(SOURCES:.c=.c.test.sil)

.PHONY: print
print: capture

.PHONY: test
test: capture
	$(QUIET)error=0; for file in $(SOURCES) ; do \
	  diff -u "$$file.sil" "$$file.test.sil" || error=1 ; \
	done ; \
	if [ $$error = 1 ]; then exit 1; fi

.PHONY: replace
replace: capture
	$(QUIET)for file in $(SOURCES) ; do \
	  mv $$file.test.sil $$file.sil ; \
	done

.PHONY: clean
clean:
	$(REMOVE_DIR) infer-out */*.test.sil */*/*.test.sil $(CLEAN_EXTRA)
