# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
INFER_OUT = infer-out
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/base.make

SOURCES = $(sort $(wildcard previous/*.py))
BASENAMES = $(notdir $(SOURCES))

default: test

# Run semdiff on all file pairs, accumulating results
issues.exp.test: $(SOURCES) $(wildcard current/*.py) $(INFER_BIN)
	$(QUIET)mkdir -p $(INFER_OUT)
	$(QUIET)rm -f $@
	$(QUIET)for f in $(BASENAMES); do \
		$(INFER_BIN) semdiff --quiet --no-progress-bar \
			--semdiff-previous previous/$$f \
			--semdiff-current current/$$f \
			--issues-tests $@ \
			-o $(INFER_OUT)/$$f.out 2>&1 || true; \
	done
	$(QUIET)sed -i 's|$(CURDIR)/||g' $@

.PHONY: analyze
analyze: issues.exp.test

.PHONY: print
print: issues.exp.test
	$(QUIET)cat $<

.PHONY: test
test: issues.exp.test
	$(QUIET)cd $(TESTS_DIR) && \
	$(call check_no_diff,$(TEST_REL_DIR)/issues.exp,$(TEST_REL_DIR)/issues.exp.test)

.PHONY: replace
replace: issues.exp.test
	cp $< issues.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) $(INFER_OUT) issues.exp.test
