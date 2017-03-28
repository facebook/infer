# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

TESTS_DIR = ../..

ANALYZER = infer

PROJECT_ROOT = ../codetoanalyze/ndk-build/hello_app
SOURCES = $(PROJECT_ROOT)/hello.c
OBJECTS = $(PROJECT_ROOT)/hello.o
INFERPRINT_OPTIONS = --issues-tests
CLEAN_EXTRA = libs obj

include $(TESTS_DIR)/infer.make

$(OBJECTS): $(SOURCES)
	cd $(PROJECT_ROOT) && \
	$(NDKBUILD) -B NDK_LIBS_OUT=./libs NDK_OUT=./obj

infer-out/report.json: $(CLANG_DEPS) $(SOURCES)
	$(QUIET)cd $(PROJECT_ROOT) && \
	$(call silent_on_success,Testing ndk-build clang integration,\
	  $(INFER_BIN) -a $(ANALYZER) --results-dir $(CURDIR)/infer-out -- \
	    $(NDKBUILD) -B NDK_LIBS_OUT=./libs NDK_OUT=./obj)
