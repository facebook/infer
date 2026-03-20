# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

INFER_OPTIONS = --config-gating-only --debug-exceptions \
  --config-gating-method getConfigFlag \
  --config-gating-method 'Config\.isEnabled' \
  --config-gating-method 'Config\.getValue'
INFERPRINT_OPTIONS = --issues-tests
SOURCES = $(wildcard *.java)

include $(TESTS_DIR)/javac.make
