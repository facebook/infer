# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# E2E test involving the test_determinator feature

TESTS_DIR = ../..
include $(TESTS_DIR)/base.make
include $(TESTS_DIR)/objc.make

A_MM = A.mm
CLANG_OPTIONS = -c $(OBJCPP_CLANG_OPTIONS) -fobjc-arc
TEST_DETERMINATOR_RESULT = infer-out-mod2/diff_determinator.json
DIFF_OUTPUT = diff.mod2.test
INFER_OPTIONS = --test-determinator --process-clang-ast --no-linters --no-capture

default: $(TEST_DETERMINATOR_RESULT)

$(DIFF_OUTPUT):
	$(QUIET)echo -n '$(A_MM):' > diff.mod1.test
	$(QUIET)(diff -N --unchanged-line-format="U" --old-line-format="O" --new-line-format="N" \
		orig-$(A_MM) mod1-$(A_MM) || [ $$? = 1 ]) >> diff.mod1.test
	$(QUIET)echo -n '$(A_MM):' > diff.mod2.test
	$(QUIET)(diff -N --unchanged-line-format="U" --old-line-format="O" --new-line-format="N" \
		orig-$(A_MM) mod2-$(A_MM) || [ $$? = 1 ]) >> diff.mod2.test

$(TEST_DETERMINATOR_RESULT): $(DIFF_OUTPUT)
	$(QUIET)$(call silent_on_success,Testing clang test-determinator with set of changes in mod1,\
		cp mod1-$(A_MM) $(A_MM);\
		$(INFER_BIN) -o infer-out-mod1 $(INFER_OPTIONS) --profiler-samples profiler_samples.json --modified-lines diff.mod1.test -- clang $(CLANG_OPTIONS) $(A_MM))
	$(QUIET)$(call silent_on_success,Testing test-determinator-clang with set of changes in mod2,\
		cp mod2-$(A_MM) $(A_MM);\
		$(INFER_BIN) -o infer-out-mod2 $(INFER_OPTIONS) --profiler-samples profiler_samples.json --modified-lines diff.mod2.test -- clang $(CLANG_OPTIONS) $(A_MM))
	$(QUIET) rm $(A_MM)

.PHONY: test
test: $(TEST_DETERMINATOR_RESULT)
	$(QUIET)$(call check_no_diff,test_determinator.json.mod1.exp,infer-out-mod1/test_determinator.json)
	$(QUIET)$(call check_no_diff,test_determinator.json.mod2.exp,infer-out-mod2/test_determinator.json)

.PHONY: replace
replace: $(TEST_DETERMINATOR_RESULT)
	$(COPY) infer-out-mod1/test_determinator.json test_determinator.json.mod1.exp
	$(COPY) infer-out-mod2/test_determinator.json test_determinator.json.mod2.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) *.test infer-out-mod* *.o
