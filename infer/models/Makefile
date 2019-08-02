# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

ROOT_DIR = ../..
include $(ROOT_DIR)/Makefile.config

C_MODELS_DIR = c/src
CPP_MODELS_DIR = cpp/src
JAVA_MODELS_DIR = java
OBJC_MODELS_DIR = objc/src
OBJCPP_MODELS_DIR = objcpp/src

MODELS_INFER_OUT = infer-out
MODELS_INFER_OPTIONS = --jobs 1 --biabduction-only --results-dir $(MODELS_INFER_OUT) --biabduction-models-mode

all:

.PHONY: clean_specs
clean_specs:
	$(REMOVE) $(SPECS_LIB_DIR)/*.specs $(MODELS_RESULTS_FILE)

# clang models specs only for now
# The clang deps have changed, so the models need to be rebuilt. If infer itself has changed, we
# need to nuke the previous specs files in case the serialization has changed, otherwise we might
# encounter a segfault reading them.
$(MODELS_RESULTS_FILE): $(MAKEFILE_LIST) $(CLANG_DEPS_NO_MODELS)
	$(QUIET)$(MAKE) clean_specs
	$(QUIET)$(REMOVE_DIR) $(MODELS_INFER_OUT)
#	[make clean] each time to recompile all the models
ifeq ($(BUILD_C_ANALYZERS),yes)
	$(QUIET)$(MAKE) -C $(C_MODELS_DIR) clean
	$(QUIET)$(MAKE) -C $(CPP_MODELS_DIR) clean
ifeq (yes, $(HAS_OBJC))
	$(QUIET)$(MAKE) -C $(OBJC_MODELS_DIR) clean
	$(QUIET)$(MAKE) -C $(OBJCPP_MODELS_DIR) clean
endif
	$(QUIET)$(call silent_on_success,Capturing C models,\
	  $(INFER_BIN) capture $(MODELS_INFER_OPTIONS) --continue -- $(MAKE) -C $(C_MODELS_DIR) all)
	$(QUIET)$(call silent_on_success,Capturing C++ models,\
	  $(INFER_BIN) capture $(MODELS_INFER_OPTIONS) --continue -- $(MAKE) -C $(CPP_MODELS_DIR) all)
ifeq (yes, $(HAS_OBJC))
	$(QUIET)$(call silent_on_success,Capturing ObjCPP models,\
	  $(INFER_BIN) capture $(MODELS_INFER_OPTIONS) --continue -- $(MAKE) -C $(OBJCPP_MODELS_DIR) all)
	$(QUIET)$(call silent_on_success,Capturing ObjC models,\
	  $(INFER_BIN) capture $(MODELS_INFER_OPTIONS) --continue -- $(MAKE) -C $(OBJC_MODELS_DIR) all)
endif
endif
	$(QUIET)$(call silent_on_success,Analyzing clang models,\
	  $(INFER_BIN) analyze $(MODELS_INFER_OPTIONS))
	$(QUIET)$(INSTALL_DATA) $(MODELS_INFER_OUT)/specs/*.specs $(SPECS_LIB_DIR)
	$(QUIET)touch $@

.PHONY: java
java:
	$(QUIET)$(MAKE) -C $@ install

ifeq ($(BUILD_C_ANALYZERS),yes)
all: $(MODELS_RESULTS_FILE)
endif
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
all: java
endif

.PHONY: clean
clean: clean_specs
	$(QUIET)$(MAKE) -C $(JAVA_MODELS_DIR) clean
	$(QUIET)$(MAKE) -C $(C_MODELS_DIR) clean
	$(QUIET)$(MAKE) -C $(CPP_MODELS_DIR) clean
ifeq (yes, $(HAS_OBJC))
	$(QUIET)$(MAKE) -C $(OBJC_MODELS_DIR) clean
	$(QUIET)$(MAKE) -C $(OBJCPP_MODELS_DIR) clean
endif
