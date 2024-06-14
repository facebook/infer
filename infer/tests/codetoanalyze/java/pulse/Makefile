# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
INFER_OPTIONS = \
  --pulse-only --debug-exceptions --pulse-nullsafe-report-npe-as-separate-issue --no-pulse-force-continue \
  --pulse-report-issues-for-tests --report-suppress-errors SENSITIVE_DATA_FLOW \
  --report-suppress-errors DATA_FLOW_TO_SINK \
  --pulse-specialization-partial \
  --pulse-transitive-access-config TransitiveAccessConfig.json \
  --pulse-monitor-transitive-callees --pulse-monitor-transitive-missed-captures
INFERPRINT_OPTIONS = --issues-tests
SOURCES = $(wildcard *.java taint/*.java)

include $(TESTS_DIR)/javac.make
