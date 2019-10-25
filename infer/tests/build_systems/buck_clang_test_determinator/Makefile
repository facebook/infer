# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
include $(TESTS_DIR)/base.make

A_C = src/a.c
B_C = src/b.c
BUCK_TARGET = //src:test
TEST_DETERMINATOR_RESULT = infer-out/test_determinator.json
DIFF_OUTPUT = diff.mod.test
INFER_OPTIONS = --buck-compilation-database no-deps --process-clang-ast --no-linters --no-capture --test-determinator \
	--modified-lines $(DIFF_OUTPUT) --profiler-samples profiler_samples.json

$(DIFF_OUTPUT):
	$(QUIET)echo -n '$(A_C):' > diff.mod.test
	$(QUIET)(diff -N --unchanged-line-format="U" --old-line-format="O" --new-line-format="N" \
		$(A_C) src/mod-a.c || [ $$? = 1 ]) >> diff.mod.test
	$(QUIET)echo >> diff.mod.test
	$(QUIET)echo -n '$(B_C):' >> diff.mod.test
	$(QUIET)(diff -N --unchanged-line-format="U" --old-line-format="O" --new-line-format="N" \
		$(B_C) src/mod-b.c || [ $$? = 1 ]) >> diff.mod.test
	$(QUIET)echo >> diff.mod.test

$(TEST_DETERMINATOR_RESULT): $(DIFF_OUTPUT)
	$(QUIET)$(call silent_on_success,Testing test-determinator for clang with set of changes,\
		$(INFER_BIN) $(INFER_OPTIONS) -- buck build --no-cache $(BUCK_TARGET))

.PHONY: test
test: $(TEST_DETERMINATOR_RESULT)
	$(QUIET)$(call check_no_diff,test_determinator.json.exp,$(TEST_DETERMINATOR_RESULT))

.PHONY: replace
replace: $(TEST_DETERMINATOR_RESULT)
	$(COPY) $(TEST_DETERMINATOR_RESULT) test_determinator.json.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) *.test infer-out* buck-out*
