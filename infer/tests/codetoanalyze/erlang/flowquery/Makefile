# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

FLOWQUERIES = $(patsubst %,--data-flow-queries-on-topl %,$(wildcard *.fq))
INFER_OPTIONS = --topl-max-disjuncts 100 --topl-only $(FLOWQUERIES) --project-root $(TESTS_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.erl)

include $(TESTS_DIR)/erlc.make

infer-out/report.json: $(MAKEFILE_LIST)
