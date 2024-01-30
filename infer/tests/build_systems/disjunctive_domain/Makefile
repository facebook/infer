# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

SRC = $(wildcard *.c)

include $(TESTS_DIR)/base.make

disjunctive.exp.test: $(INFER_BIN) $(SRC) $(MAKEFILE_LIST)
	$(QUIET)echo "The order of disjuncts for each function should remain stable" > $@
	$(QUIET)echo "This tests that the set of paths explored by the disjunctive analysis" >> $@
	$(QUIET)echo "monotonically increases as the disjunct limit increases" >> $@
	$(QUIET)echo "" >> $@
	$(QUIET)echo "N_disjuncts = 1" >> $@
	$(QUIET)echo "" >> $@
	$(QUIET)$(INFER_BIN) -j 1 --disjunctive-demo-only --pulse-max-disjuncts 1 -- clang -c $(SRC) >> $@
	$(QUIET)echo "" >> $@
	$(QUIET)echo "----------------------------------------" >> $@
	$(QUIET)echo "N_disjuncts = 4" >> $@
	$(QUIET)echo "" >> $@
	$(QUIET)$(INFER_BIN) -j 1 --disjunctive-demo-only --pulse-max-disjuncts 4 >> $@
	$(QUIET)echo "" >> $@
	$(QUIET)echo "----------------------------------------" >> $@
	$(QUIET)echo "N_disjuncts = 10" >> $@
	$(QUIET)echo "" >> $@
	$(QUIET)$(INFER_BIN) -j 1 --disjunctive-demo-only --pulse-max-disjuncts 10 >> $@

.PHONY: print
print: disjunctive.exp.test

.PHONY: test
test: disjunctive.exp.test
	$(QUIET)diff -u disjunctive.exp $<

.PHONY: replace
replace: disjunctive.exp.test
	$(QUIET)$(COPY) $< disjunctive.exp

.PHONY: clean
clean:
	$(QUIET)$(REMOVE_DIR) infer-out disjunctive.exp.test $(SRC:.c=.o)
