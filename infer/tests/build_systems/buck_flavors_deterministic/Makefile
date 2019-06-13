# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

BUCK_TARGET = //src:hello
SOURCES = $(wildcard src/hello.c)
OBJECTS = buck-out/gen/src/hello\#compile-hello.c.o1f717d69,default/hello.c.o
INFER_OPTIONS = --report-custom-error --project-root $(TESTS_DIR) --no-linters
INFERPRINT_OPTIONS = --project-root $(TESTS_DIR) --issues-tests
CLEAN_EXTRA = buck-out capture_hash-1.sha capture_hash-2.sha

include $(TESTS_DIR)/base.make

$(OBJECTS): $(JAVA_SOURCE_FILES)
	$(QUIET)$(call silent_on_success,Compiling Buck flavors tests,\
	$(BUCK) build --no-cache $(BUCK_TARGET))

differences.exp.test: $(CLANG_DEPS) $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(REMOVE_DIR) buck-out && \
	$(call silent_on_success,Running Buck flavors capture a first time,\
	$(INFER_BIN) $(INFER_OPTIONS) capture --flavors --results-dir $(CURDIR)/infer-out -- \
	  $(BUCK) build --no-cache $(BUCK_TARGET) && \
	find buck-out/gen/src/infer-out-* -type f | xargs cat | $(SHASUM) > capture_hash-1.sha)
	$(QUIET)$(REMOVE_DIR) buck-out && \
	$(call silent_on_success,Running Buck flavors capture a second time,\
	$(INFER_BIN) $(INFER_OPTIONS) capture --flavors --results-dir $(CURDIR)/infer-out -- \
	  $(BUCK) build --no-cache $(BUCK_TARGET) && \
	find buck-out/gen/src/infer-out-* -type f | xargs cat | $(SHASUM) > capture_hash-2.sha)
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
	$(QUIET)$(REMOVE_DIR) infer-out $(OBJECTS) $(CLEAN_EXTRA)
