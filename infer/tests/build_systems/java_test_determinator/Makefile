# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# E2E test involving the test_determinator feature

TESTS_DIR = ../..
include $(TESTS_DIR)/base.make

MY_FAVOURITE_CLASS_TWO = MyFavouriteClassTwo.java
TEST_DETERMINATOR_RESULT = infer-out-mod2/test_determinator.json
DIFF_OUTPUT = diff.mod2.test

default: $(TEST_DETERMINATOR_RESULT)

$(DIFF_OUTPUT):
	$(QUIET)echo -n '$(MY_FAVOURITE_CLASS_TWO):' > diff.mod1.test
	$(QUIET)(diff -N --unchanged-line-format="U" --old-line-format="O" --new-line-format="N" \
		$(MY_FAVOURITE_CLASS_TWO).original $(MY_FAVOURITE_CLASS_TWO).mod1 || [ $$? = 1 ]) >> diff.mod1.test
	$(QUIET)echo -n '$(MY_FAVOURITE_CLASS_TWO):' > diff.mod2.test
	$(QUIET)(diff -N --unchanged-line-format="U" --old-line-format="O" --new-line-format="N" \
		$(MY_FAVOURITE_CLASS_TWO).original $(MY_FAVOURITE_CLASS_TWO).mod2 || [ $$? = 1 ]) >> diff.mod2.test

$(TEST_DETERMINATOR_RESULT): $(DIFF_OUTPUT)
	$(QUIET)$(call silent_on_success,Testing test-determinator with set of changes in mod1,\
		$(INFER_BIN) -o infer-out-mod1 --test-determinator --profiler-samples profiler_samples.json \
		--modified-lines diff.mod1.test --method-decls-info methods.json.mod1)
	$(QUIET)$(call silent_on_success,Testing test-determinator with set of changes in mod2,\
		$(INFER_BIN) -o infer-out-mod2 --test-determinator --profiler-samples profiler_samples.json \
		--modified-lines diff.mod2.test --method-decls-info methods.json.mod2)

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
	$(REMOVE_DIR) *.test infer-out-mod*
