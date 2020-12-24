# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
include $(TESTS_DIR)/base.make

A_CPP = src/a.cpp
B_CPP = src/b.cpp
BUCK_TARGET = //src:test
TEST_DETERMINATOR_RESULT = infer-out/changed_functions.json
DIFF_OUTPUT = diff.mod.test
INFER_OPTIONS = --buck-clang --process-clang-ast --no-linters --no-capture --export-changed-functions --modified-lines $(DIFF_OUTPUT)

$(DIFF_OUTPUT):
	$(QUIET)echo -n '$(A_CPP):' > diff.mod.test
	$(QUIET)(diff -N --unchanged-line-format="U" --old-line-format="O" --new-line-format="N" \
		$(A_CPP) src/mod-a.cpp || [ $$? = 1 ]) >> diff.mod.test
	$(QUIET)echo >> diff.mod.test
	$(QUIET)echo -n '$(B_CPP):' >> diff.mod.test
	$(QUIET)(diff -N --unchanged-line-format="U" --old-line-format="O" --new-line-format="N" \
		$(B_CPP) src/mod-b.cpp || [ $$? = 1 ]) >> diff.mod.test
	$(QUIET)echo >> diff.mod.test

$(TEST_DETERMINATOR_RESULT): $(DIFF_OUTPUT)
	$(QUIET)$(BUCK) clean
	$(call silent_on_success,Testing export-changed-functions with set of changes,\
	  $(INFER_BIN) $(INFER_OPTIONS) -- $(BUCK) build --no-cache $(BUCK_TARGET))

.PHONY: test
test: $(TEST_DETERMINATOR_RESULT)
	$(call check_no_diff,changed_functions.json.exp,$(TEST_DETERMINATOR_RESULT))

.PHONY: replace
replace: $(TEST_DETERMINATOR_RESULT)
	$(COPY) $(TEST_DETERMINATOR_RESULT) changed_functions.json.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) *.test infer-out* buck-out*
