# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../../..

CLANG_OPTIONS = -c
INFER_OPTIONS = --topl-only --topl-properties property.topl --enable-issue-type TOPL_ERROR_LATENT --project-root $(TESTS_DIR)
INFERPRINT_OPTIONS = --issues-tests

SOURCES = $(wildcard *.c)

include $(TESTS_DIR)/clang.make

infer-out/report.json: $(MAKEFILE_LIST) property.topl
