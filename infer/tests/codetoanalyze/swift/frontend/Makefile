# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR=../../..

SWIFT_OPTIONS =
INFER_OPTIONS =

SOURCES = $(wildcard *.swift)

CLEAN_EXTRA = $(foreach source,$(SOURCES),$(basename $(notdir $(source))).o)

include $(TESTS_DIR)/base.make
include $(TESTS_DIR)/swift-base.make

default: test

%.test.sil: %.swift $(SWIFT_DEPS) $(HEADERS)
	$(QUIET)$(call silent_on_success,Testing the infer/swift frontend in $(TEST_REL_DIR),\
	  $(INFER_BIN) capture --frontend-tests --dump-textual --project-root $(TESTS_DIR) $(INFER_OPTIONS) -- \
	    $(SWIFTC) $(SWIFT_OPTIONS) $<)

capture: $(SOURCES:.swift=.test.sil)

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
