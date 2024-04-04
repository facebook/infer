# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

INFER_OPTIONS = --pulse-only --debug-exceptions --pulse-specialization-partial \
  --pulse-monitor-transitive-callees --pulse-monitor-transitive-missed-captures \
  --pulse-max-disjuncts 8 --log-pulse-unreachable-nodes \
  --pulse-transitive-access-config transitive-access-config.json \
  --pulse-transitive-access-config transitive-access-config-extra.json
INFERPRINT_OPTIONS = --issues-tests

HH_SOURCES = $(sort $(wildcard hh/*.hack))
SOURCES = $(sort $(wildcard *.hack))

include $(TESTS_DIR)/hack.make
