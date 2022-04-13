# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

# NOTE: This test is similar to fb-config-impact-paths, but gives the paths option for strict-beta
# mode.  We introduced a separate test here because strict and strict-beta modes cannot run together
# when both paths for them are given.

INFER_OPTIONS = --config-impact-analysis-only \
  --config-impact-strict-beta-mode-paths '.*/InStrictBetaModePaths.java' \
  --debug-exceptions --report-force-relative-path
INFERPRINT_OPTIONS = --issues-tests
SOURCES = $(wildcard *.java)

include $(TESTS_DIR)/javac.make
include $(TESTS_DIR)/config-impact.make
