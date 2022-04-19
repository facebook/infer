# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# E2E test involving the test_determinator feature

TESTS_DIR = ../..
include $(TESTS_DIR)/base.make

A_CPP = A.cpp
B_CPP = B.cpp
TEST_DETERMINATOR_RESULT = infer-out-mod2/diff_determinator.json
DIFF_OUTPUT = diff.mod1.test diff.mod2.test
INFER_OPTIONS = --export-changed-functions --process-clang-ast --no-linters --no-capture --buck-compilation-database 'no-deps'

default: $(TEST_DETERMINATOR_RESULT)

diff.mod1.test: orig-$(A_CPP) orig-$(B_CPP) mod1-$(A_CPP) mod1-$(B_CPP)
	$(QUIET)echo -n '$(A_CPP):' > diff.mod1.test
	$(QUIET)(diff -N --unchanged-line-format="U" --old-line-format="O" --new-line-format="N" \
		orig-$(A_CPP) mod1-$(A_CPP) || [ $$? = 1 ]) >> diff.mod1.test
	$(QUIET)echo >> diff.mod1.test
	$(QUIET)echo -n '$(B_CPP):' >> diff.mod1.test
	$(QUIET)(diff -N --unchanged-line-format="U" --old-line-format="O" --new-line-format="N" \
		orig-$(B_CPP) mod1-$(B_CPP) || [ $$? = 1 ]) >> diff.mod1.test
	$(QUIET)echo >> diff.mod1.test

diff.mod2.test: orig-$(A_CPP) mod2-$(A_CPP)
	$(QUIET)echo -n '$(A_CPP):' > diff.mod2.test
	$(QUIET)(diff -N --unchanged-line-format="U" --old-line-format="O" --new-line-format="N" \
		orig-$(A_CPP) mod2-$(A_CPP) || [ $$? = 1 ]) >> diff.mod2.test
	$(QUIET)echo >> diff.mod2.test

$(TEST_DETERMINATOR_RESULT): $(DIFF_OUTPUT)
	$(QUIET)$(call silent_on_success,Testing export-changed-functions with set of changes in mod1,\
		cp mod1-$(A_CPP) $(A_CPP);\
		cp mod1-$(B_CPP) $(B_CPP);\
		$(INFER_BIN) -o infer-out-mod1 $(INFER_OPTIONS) --modified-lines diff.mod1.test -- clang -c $(A_CPP);\
		$(INFER_BIN) -o infer-out-mod1 $(INFER_OPTIONS) --continue --modified-lines diff.mod1.test -- clang -c $(B_CPP))
	$(QUIET)$(call silent_on_success,Testing test-determinator-clang with set of changes in mod2,\
		cp mod2-$(A_CPP) $(A_CPP);\
		$(INFER_BIN) -o infer-out-mod2 $(INFER_OPTIONS) --modified-lines diff.mod2.test -- clang -c $(A_CPP))
	$(QUIET) rm $(A_CPP) $(B_CPP)

.PHONY: test
test: $(TEST_DETERMINATOR_RESULT)
	$(QUIET)$(call check_no_diff,changed_functions.json.mod1.exp,infer-out-mod1/changed_functions.json)
	$(QUIET)$(call check_no_diff,changed_functions.json.mod2.exp,infer-out-mod2/changed_functions.json)

.PHONY: replace
replace: $(TEST_DETERMINATOR_RESULT)
	$(COPY) infer-out-mod1/changed_functions.json changed_functions.json.mod1.exp
	$(COPY) infer-out-mod2/changed_functions.json changed_functions.json.mod2.exp

.PHONY: clean
clean:
	$(REMOVE_DIR) *.test infer-out-mod* *.o
