# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

# see explanations in cpp/biabduction/Makefile for the custom isystem
INFER_OPTIONS = --pulse-only --debug-exceptions --project-root $(TESTS_DIR) \
  --pulse-report-latent-issues --print-types
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard src/*.erl)

include $(TESTS_DIR)/erlang.make

infer-out/report.json: $(MAKEFILE_LIST)
