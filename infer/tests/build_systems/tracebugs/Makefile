# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

include $(TESTS_DIR)/base.make

SOURCES = ../codetoanalyze/hello.c
OBJECTS = $(SOURCES:.c=.o)

default: print

infer-out/report.json: $(SOURCES) $(CLANG_DEPS)
	$(QUIET)$(call silent_on_success,Running infer,\
	$(INFER_BIN) run -- clang -c $(SOURCES))

.PHONY: test1
test1: infer-out/report.json
	$(QUIET)$(call silent_on_success,Testing infer-explore: --max-nesting=3,\
	$(INFER_BIN) explore -o infer-out \
	  --select 0 --max-nesting 3)

.PHONY: test2
test2: infer-out/report.json
	$(QUIET)$(call silent_on_success,Testing infer-explore: --max-nesting=0,\
	$(INFER_BIN) explore -o infer-out \
	  --select 0 --max-nesting 0)

.PHONY: test3
test3: infer-out/report.json
	$(QUIET)$(call silent_on_success,Testing infer-explore: --no-source-preview,\
	$(INFER_BIN) explore -o infer-out \
	  --select 0 --no-source-preview)

.PHONY: test4
test4: infer-out/report.json
	$(QUIET)$(call silent_on_success,Testing infer-explore: --only-show,\
	$(INFER_BIN) explore -o infer-out \
	  --only-show)


.PHONY: print
print: test1 test2 test3 test4

.PHONY: test
test: print

.PHONY: replace
replace: test

.PHONY: clean
clean:
	$(REMOVE_DIR) $(OBJECTS) infer-out $(CLEAN_EXTRA)
