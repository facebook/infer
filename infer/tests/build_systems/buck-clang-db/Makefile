# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..

SOURCE_DIR = ../codetoanalyze

ANALYZER = checkers
INFER_OPTIONS = --report-custom-error --developer-mode --project-root $(SOURCE_DIR)
SOURCES = $(wildcard $(SOURCE_DIR)/*.cpp $(SOURCE_DIR)/*.h)
OBJECTS = '$(SOURCE_DIR)/buck-out/gen/clang_compilation_database/__Hel lo\#compilation-database,x86_64.json'
INFERPRINT_OPTIONS = --issues-tests
CLEAN_EXTRA = $(SOURCE_DIR)/buck-out

include $(TESTS_DIR)/infer.make

infer-out/report.json: $(CLANG_DEPS) $(SOURCES)
	$(QUIET)cd $(SOURCE_DIR) && \
	$(call silent_on_success,Testing Buck Clang compilation database integration,\
	  $(INFER_BIN) -a $(ANALYZER) $(INFER_OPTIONS) -o $(CURDIR)/$(@D) \
	  --buck-compilation-database no-deps \
	  -- $(BUCK) build --no-cache '//clang_compilation_database:Hel lo#default' @clang_compilation_database/buck_target_hello_test.txt)
