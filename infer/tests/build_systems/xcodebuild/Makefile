# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..

ANALYZER = infer
APP_DIR = ../codetoanalyze/xcodebuild/simple_app
SOURCES = $(wildcard $(APP_DIR)/simple_app/*.m)
OBJECTS = $(APP_DIR)/app_built
INFERPRINT_OPTIONS = --issues-tests

include $(TESTS_DIR)/infer.make

XCODEBUILD = xcodebuild -verbose -target simple_app -configuration Release -sdk iphonesimulator

$(OBJECTS): $(SOURCES)
	cd $(APP_DIR) && \
	$(XCODEBUILD) && \
	touch app_built

infer-out/report.json: $(CLANG_DEPS) $(SOURCES)
	$(QUIET)cd $(APP_DIR) && \
	$(XCODEBUILD) clean && \
	$(call silent_on_success,Testing xcodebuild default integration,\
	  $(INFER_BIN) -a $(ANALYZER) $(INFER_OPTIONS) --project-root $(TESTS_DIR) \
	    --results-dir $(CURDIR)/$(@D) -- $(XCODEBUILD))

clean:
	cd $(APP_DIR) && \
	$(XCODEBUILD) clean
	$(REMOVE_DIR) infer-out issues.exp.test
