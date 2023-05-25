# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

BUCK_TARGET = //src:hello
SOURCES = $(wildcard src/hello.c)
INFER_OPTIONS = --report-custom-error
INFERPRINT_OPTIONS = --issues-tests
CLEAN_EXTRA = buck-out capture_hash-1.sha capture_hash-2.sha

include $(TESTS_DIR)/base.make

differences.exp.test: $(CLANG_DEPS) $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(BUCK) clean && \
	$(call silent_on_success,Running Buck flavors capture a first time,\
	$(INFER_BIN) $(INFER_OPTIONS) capture --buck-clang --results-dir $(CURDIR)/infer-out -- \
	  $(BUCK) build --no-cache $(BUCK_TARGET) && \
	  find buck-out/gen/src -path "*infer-out*" -type f | sort | xargs cat | $(SHASUM) > capture_hash-1.sha)
	$(QUIET)$(BUCK) clean && \
	$(call silent_on_success,Running Buck flavors capture a second time,\
	$(INFER_BIN) $(INFER_OPTIONS) capture --buck-clang --results-dir $(CURDIR)/infer-out -- \
	  $(BUCK) build --no-cache $(BUCK_TARGET) && \
	  find buck-out/gen/src -path "*infer-out*" -type f | sort | xargs cat | $(SHASUM) > capture_hash-2.sha)
	$(QUIET)$(call silent_on_success,Computing difference,\
	diff capture_hash-1.sha capture_hash-2.sha && \
	  echo "capture is deterministic" > $@ || \
	  echo "capture is not deterministic!" > $@; \
	)
	$(QUIET)$(REMOVE) capture_hash-1.sha capture_hash-2.sha

.PHONY: print
print: differences.exp.test

.PHONY: test
test: differences.exp.test
	$(QUIET)diff -u differences.exp $<

.PHONY: replace
replace: differences.exp.test
	$(QUIET)cp $< differences.exp

.PHONY: clean
clean:
	$(QUIET)$(REMOVE_DIR) infer-out differences.exp.test $(CLEAN_EXTRA)
