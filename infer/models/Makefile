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

CLANG_MODELS_FILE = $(SPECS_LIB_DIR)/clang_models

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

.PHONY: java clang clean clean_specs $(CLANG_SUBDIRS)

clean_specs:
	$(REMOVE) $(SPECS_LIB_DIR)/*.specs $(C_MODELS_FILE) $(CPP_MODELS_FILE) $(OBJC_MODELS_FILE)

$(CLANG_SUBDIRS):
	$(QUIET)$(MAKE) -C $@ install

# The clang deps have changed, so the models need to be rebuilt. If infer itself has changed, we
# need to nuke the previous specs files in case the serialization has changed, otherwise we might
# encounter a segfault reading them.
$(CLANG_MODELS_FILE): $(CLANG_DEPS_NO_MODELS)
	$(QUIET)$(MAKE) clean_specs
#	Since [CLANG_DEPS_NO_MODELS] is newer than [CLANG_MODELS_FILE], it is also newer than the
#	previously built clang models, so they will all be rebuilt by the subdirs' Makefiles.
	$(QUIET)$(MAKE) $(CLANG_SUBDIRS)
#	Maintain the invariant that [CLANG_MODELS_FILE] is newer than the clang models.
	$(QUIET)touch $@

clang: $(CLANG_MODELS_FILE)
#	Run the sub-makes unconditionally since they know more about when they need to be
#	rebuilt. For instance, they need to be rebuilt when their source files change, which is not
#	taken into account by the dependencies of the $(CLANG_MODELS_FILE) target. If we rebuilt
#	$(CLANG_MODELS_FILE) already, then the command below will be a no-op (so we don't rebuild
#	the models twice).
	$(QUIET)$(MAKE) $(CLANG_SUBDIRS)

java:
	$(QUIET)$(MAKE) -C $@ install

clean: clean_specs
	$(QUIET)$(MAKE) -C $(JAVA_MODELS_DIR) clean
	$(QUIET)$(MAKE) -C $(C_MODELS_DIR) clean
	$(QUIET)$(MAKE) -C $(CPP_MODELS_DIR) clean
ifneq (no, $(XCODE_SELECT))
	$(QUIET)$(MAKE) -C $(OBJC_MODELS_DIR) clean
endif
