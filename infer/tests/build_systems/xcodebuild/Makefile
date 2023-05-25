# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

APP_DIR = ../codetoanalyze/xcodebuild/simple_app
SOURCES = $(wildcard $(APP_DIR)/simple_app/*.m)
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/infer.make

XCODEBUILD = xcodebuild -verbose -target simple_app -configuration Release -sdk iphonesimulator

infer-out/report.json: $(CLANG_DEPS) $(SOURCES)
	$(QUIET)cd $(APP_DIR) && \
	$(XCODEBUILD) clean && \
	$(call silent_on_success,Testing xcodebuild default integration,\
	  $(INFER_BIN) $(INFER_OPTIONS) --project-root $(TESTS_DIR) \
	    --results-dir $(CURDIR)/$(@D) -- $(XCODEBUILD))

clean:
	$(REMOVE_DIR) infer-out issues.exp.test $(APP_DIR)/app_built $(APP_DIR)/build
