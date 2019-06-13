# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

PROJECT_ROOT = ../codetoanalyze/ndk-build/hello_app
SOURCES = $(PROJECT_ROOT)/hello.c
OBJECTS = $(PROJECT_ROOT)/hello.o
INFERPRINT_OPTIONS = --issues-tests
CLEAN_EXTRA = $(PROJECT_ROOT)/libs $(PROJECT_ROOT)/obj

include $(TESTS_DIR)/infer.make

$(OBJECTS): $(SOURCES)
	cd $(PROJECT_ROOT) && \
	$(NDKBUILD) -B NDK_LIBS_OUT=./libs NDK_OUT=./obj

infer-out/report.json: $(CLANG_DEPS) $(SOURCES)
	$(QUIET)cd $(PROJECT_ROOT) && \
	$(call silent_on_success,Testing ndk-build clang integration,\
	  $(INFER_BIN) --results-dir $(CURDIR)/infer-out -- \
	    $(NDKBUILD) -B NDK_LIBS_OUT=./libs NDK_OUT=./obj V=1)
