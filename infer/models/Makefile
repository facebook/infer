# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = ../..
include $(ROOT_DIR)/Makefile.config

C_MODELS_DIR = c
CPP_MODELS_DIR = cpp
JAVA_MODELS_DIR = java
OBJC_MODELS_DIR = objc

CLANG_SUBDIRS = $(C_MODELS_DIR) $(CPP_MODELS_DIR)
ifneq (no, $(XCODE_SELECT))
CLANG_SUBDIRS += $(OBJC_MODELS_DIR)
endif

all:
ifeq ($(BUILD_C_ANALYZERS),yes)
all: clang
endif
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
all: java
endif

.PHONY: java clang clean $(CLANG_SUBDIRS)

clean_specs:
	$(REMOVE) $(SPECS_LIB_DIR)/*.specs

$(CLANG_SUBDIRS):
	$(MAKE) -C $@ install

clang: clean_specs
	$(MAKE) $(CLANG_SUBDIRS)

java: clean_specs
	$(MAKE) -C $@ install

clean: clean_specs
	$(MAKE) -C $(JAVA_MODELS_DIR) clean
	$(MAKE) -C $(C_MODELS_DIR) clean
	$(MAKE) -C $(CPP_MODELS_DIR) clean
ifneq (no, $(XCODE_SELECT))
	$(MAKE) -C $(OBJC_MODELS_DIR) clean
endif
