# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..
INFER_OPTIONS = --debug-exceptions --pulse-only --dependencies
INFERPRINT_OPTIONS = --issues-tests
SOURCES = my/Application.java

include $(TESTS_DIR)/javac.make

infer-out/report.json: .inferconfig