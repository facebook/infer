# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

CLEAN_EXTRA = $(foreach source,$(SOURCES),$(basename $(notdir $(source))).o)

include $(TESTS_DIR)/base.make
include $(TESTS_DIR)/clang-base.make

ONE_SOURCE = $(lastword $(SOURCES))

default: test

$(ONE_SOURCE).test.dot: $(CLANG_DEPS) $(SOURCES) $(HEADERS)
	$(QUIET)$(call silent_on_success,Testing the infer/clang frontend in $(TEST_REL_DIR),\
	  $(INFER_BIN) capture --frontend-tests --project-root $(TESTS_DIR) $(INFER_OPTIONS) -- \
	    clang $(CLANG_OPTIONS) $(SOURCES))

.PHONY: capture
capture: $(ONE_SOURCE).test.dot

.PHONY: print
print: capture

.PHONY: test
test: capture
	$(QUIET)error=0; for file in $(SOURCES) ; do \
	  diff -u "$$file.dot" "$$file.test.dot" || error=1 ; \
	done ; \
	if [ $$error = 1 ]; then exit 1; fi

.PHONY: replace
replace: capture
	$(QUIET)for file in $(SOURCES) ; do \
	  mv $$file.test.dot $$file.dot ; \
	done

.PHONY: clean
clean:
	$(REMOVE_DIR) infer-out */*.test.dot */*/*.test.dot $(CLEAN_EXTRA)
