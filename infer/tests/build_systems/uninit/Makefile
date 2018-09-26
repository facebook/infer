# Copyright (c) 2017-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

# see explanations in cpp/errors/Makefile for the custom isystem
CLANG_OPTIONS = -x c++ -std=c++11 -nostdinc++ -isystem$(MODELS_DIR)/cpp/include -isystem$(CLANG_INCLUDES)/c++/v1/ -c
INFER_OPTIONS = --uninit-only --uninit-interproc --debug-exceptions --project-root $(TESTS_DIR)
INFERPRINT_OPTIONS = --issues-tests
SOURCES = $(wildcard *.cpp)

include $(TESTS_DIR)/infer.make

infer-out/report.json: $(MAKEFILE_LIST)

infer-out/report.json: $(CLANG_DEPS) $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(REMOVE_DIR) buck-out && \
	$(call silent_on_success,Testing uninit analysis with inter-procedural mode,\
	$(INFER_BIN) $(INFER_OPTIONS) --results-dir $(CURDIR)/infer-out -- clang $(CLANG_OPTIONS) $(SOURCES))
