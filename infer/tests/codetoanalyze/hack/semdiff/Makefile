# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
INFER_OUT = infer-out
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/base.make

# Each subdirectory contains a config file and previous/current test pairs
CONFIGS = $(sort $(wildcard */*.py))
SUBDIRS = $(patsubst %/,%,$(dir $(CONFIGS)))

HH_PARSE = $(shell command -v hh_parse 2>/dev/null)

default: test

# Run semdiff on all subdirectories, each with its own config
issues.exp.test: $(wildcard */*.py) $(wildcard */previous/*.php) $(wildcard */current/*.php) $(INFER_BIN)
ifndef HH_PARSE
	$(QUIET)echo "hh_parse not found, skipping Hack semdiff tests"
	$(QUIET)cp issues.exp $@
else
	$(QUIET)mkdir -p $(INFER_OUT)
	$(QUIET)rm -f $@
	$(QUIET)for subdir in $(SUBDIRS); do \
		config=$$(ls $$subdir/*.py) ; \
		for f in $$(ls $$subdir/previous/*.php 2>/dev/null | sort); do \
			base=$$(basename $$f) ; \
			$(INFER_BIN) semdiff --quiet --no-progress-bar \
				--semdiff-configuration $$config \
				--semdiff-previous $$subdir/previous/$$base \
				--semdiff-current $$subdir/current/$$base \
				--issues-tests $@ \
				-o $(INFER_OUT)/$$subdir.$$base.out 2>&1 || true; \
		done ; \
	done
	$(QUIET)sed -i 's|$(CURDIR)/||g' $@
endif

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
