# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

SOURCE_DIR = ../codetoanalyze

INFER_OPTIONS = --report-custom-error --developer-mode --project-root $(SOURCE_DIR)
SOURCES = $(wildcard $(SOURCE_DIR)/*.cpp $(SOURCE_DIR)/*.h)
INFERPRINT_OPTIONS = --issues-tests
CLEAN_EXTRA = $(SOURCE_DIR)/buck-out

include $(TESTS_DIR)/infer.make

infer-out/report.json: $(CLANG_DEPS) $(SOURCES)
	$(QUIET)cd $(SOURCE_DIR) && \
	$(BUCK) clean && \
	$(call silent_on_success,Testing Buck Clang compilation database integration,\
	  $(INFER_BIN) $(INFER_OPTIONS) -o $(CURDIR)/$(@D) \
	  --buck-compilation-database no-deps \
	  -- $(BUCK) build --no-cache '//clang_compilation_database:Hel lo#default' @clang_compilation_database/buck_target_hello_test.txt)
