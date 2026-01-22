# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR=../../..

# -whole-object-optimization is required to make for multi-file capture
SWIFT_OPTIONS = -whole-module-optimization
INFER_OPTIONS =

SOURCES = $(realpath $(wildcard *.swift))

SIL_FILES = $(SOURCES:.swift=.swift.test.sil)

CLEAN_EXTRA = *.o *.dot *.test.sil

include $(TESTS_DIR)/base.make

.NOTPARALLEL:

default: test

$(SIL_FILES): $(SOURCES) $(SWIFT_DEPS)
	$(QUIET)$(call silent_on_success,Testing the infer/swift frontend in $(TEST_REL_DIR),\
	  $(INFER_BIN) capture --frontend-tests --dump-textual $(INFER_OPTIONS) -- \
	    $(SWIFTC) $(SWIFT_OPTIONS) $(SOURCES))

capture: $(SIL_FILES)

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
	$(REMOVE_DIR) infer-out *.test.sil $(CLEAN_EXTRA)
