# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

INFER_OPTIONS = \
  --pulse-only --erlang-with-otp-specs --debug-exceptions --no-pulse-force-continue \
  --project-root $(TESTS_DIR) --print-types \
  --report-suppress-errors MUTUAL_RECURSION_CYCLE \

INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.erl)

include $(TESTS_DIR)/erlc.make

infer-out/report.json: $(MAKEFILE_LIST)
