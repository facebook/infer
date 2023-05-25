# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..
ROOT_DIR = $(TESTS_DIR)/../..

BUCK_TARGET = //src:hello
SOURCES = src/hello.c src/hello2.c
INFER_OPTIONS = --report-custom-error --developer-mode --buck-clang
INFERPRINT_OPTIONS = --issues-tests
CLEAN_EXTRA = buck-out

include $(TESTS_DIR)/infer.make

infer-out/report.json: $(CLANG_DEPS) $(SOURCES) $(MAKEFILE_LIST)
	$(QUIET)$(BUCK) clean
	$(call silent_on_success,Testing infer-run Buck flavors integration,\
	$(INFER_BIN) $(INFER_OPTIONS) run --results-dir $(CURDIR)/infer-out --reactive -- \
	  $(BUCK) build --no-cache $(BUCK_TARGET))
