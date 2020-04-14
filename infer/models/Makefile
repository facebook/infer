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

CLANG_RESULTS_DIR = infer-out-clang
CLANG_INFER_OPTIONS = --jobs 1 --biabduction-only --results-dir $(CLANG_RESULTS_DIR) --biabduction-models-mode

JAVA_MODELS_OUT = java/models
JAVA_RESULTS_DIR = infer-out-java
INFER_REPORT = $(JAVA_RESULTS_DIR)/report.json
MODELS_JAR = models.jar

JAVA_SOURCES = $(shell find $(JAVA_BUILTINS_DIR) $(JAVA_MODELS_DIR) -name "*.java")

MODELS_CLASSPATH = $(ANDROID_JAR):$(GUAVA_JAR):$(JACKSON_JAR):$(JSR_305_JAR):$(INFER_ANNOTATIONS_JAR)

.PHONY: all
all: $(MODELS_RESULTS_FILE)

.PHONY: clean_specs
clean_specs:
	$(REMOVE) $(SPECS_LIB_DIR)/*.specs $(MODELS_RESULTS_FILE)

clang:
	$(QUIET)$(REMOVE_DIR) $(CLANG_RESULTS_DIR)
#	[make clean] each time to recompile all the models
ifeq ($(BUILD_C_ANALYZERS),yes)
	$(QUIET)$(MAKE) -C $(C_MODELS_DIR) clean
	$(QUIET)$(MAKE) -C $(CPP_MODELS_DIR) clean
ifeq (yes, $(HAS_OBJC))
	$(QUIET)$(MAKE) -C $(OBJC_MODELS_DIR) clean
	$(QUIET)$(MAKE) -C $(OBJCPP_MODELS_DIR) clean
endif
	$(QUIET)$(call silent_on_success,Capturing C models,\
	  $(INFER_BIN) capture $(CLANG_INFER_OPTIONS) --continue -- $(MAKE) -C $(C_MODELS_DIR) all)
	$(QUIET)$(call silent_on_success,Capturing C++ models,\
	  $(INFER_BIN) capture $(CLANG_INFER_OPTIONS) --continue -- $(MAKE) -C $(CPP_MODELS_DIR) all)
ifeq (yes, $(HAS_OBJC))
	$(QUIET)$(call silent_on_success,Capturing ObjCPP models,\
	  $(INFER_BIN) capture $(CLANG_INFER_OPTIONS) --continue -- $(MAKE) -C $(OBJCPP_MODELS_DIR) all)
	$(QUIET)$(call silent_on_success,Capturing ObjC models,\
	  $(INFER_BIN) capture $(CLANG_INFER_OPTIONS) --continue -- $(MAKE) -C $(OBJC_MODELS_DIR) all)
endif
endif
	$(QUIET)$(call silent_on_success,Analyzing clang models,\
	  $(INFER_BIN) analyze $(CLANG_INFER_OPTIONS))
	$(QUIET)$(INSTALL_DATA) $(CLANG_RESULTS_DIR)/specs/*.specs $(SPECS_LIB_DIR)

.PHONY: java
java:
	$(QUIET)rm -fr $(JAVA_MODELS_OUT)
	$(QUIET)$(MKDIR_P) $(JAVA_MODELS_OUT)
	$(QUIET)rm -f $(JAVA_MODELS_JAR)
	$(QUIET)$(call silent_on_success,Building Java models,\
	  $(INFER_BIN) --biabduction-only --results-dir $(JAVA_RESULTS_DIR) --biabduction-models-mode -- \
	  $(JAVAC) -bootclasspath $(ANDROID_JAR) -d $(JAVA_MODELS_OUT) -classpath $(MODELS_CLASSPATH) \
	    $(JAVA_SOURCES) -source 7 -target 7)
	cd $(JAVA_MODELS_OUT); jar cf ../../$(MODELS_JAR) *
#	install .specs files into the models directory
	$(QUIET)$(INSTALL_DATA) $(JAVA_RESULTS_DIR)/specs/*.specs $(SPECS_LIB_DIR)
#	install the models.jar file too
	$(INSTALL_DATA) -C $(MODELS_JAR) $(JAVA_MODELS_JAR)
	$(QUIET)touch $(JAVA_MODELS_JAR)


ALL_MODELS=
ifeq ($(BUILD_C_ANALYZERS),yes)
ALL_MODELS += clang
endif
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
ALL_MODELS += java
endif

# If the models deps have changed then the models need to be rebuilt. If infer itself has changed,
# we need to nuke the previous specs files in case the serialization has changed, otherwise we might
# encounter a segfault reading them.
$(MODELS_RESULTS_FILE): $(MAKEFILE_LIST)
#	clean first
	$(MAKE) clean_specs
#	build java and clang models in parallel
	$(MAKE) $(ALL_MODELS)
	$(QUIET)touch $@

ifeq ($(BUILD_C_ANALYZERS),yes)
$(MODELS_RESULTS_FILE): $(CLANG_DEPS_NO_MODELS)
endif
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
$(MODELS_RESULTS_FILE): $(JAVA_DEPS_NO_MODELS) $(JAVA_SOURCES)
endif

.PHONY: clean
clean: clean_specs
	$(QUIET)$(MAKE) -C $(C_MODELS_DIR) clean
	$(QUIET)$(MAKE) -C $(CPP_MODELS_DIR) clean
ifeq (yes, $(HAS_OBJC))
	$(QUIET)$(MAKE) -C $(OBJC_MODELS_DIR) clean
	$(QUIET)$(MAKE) -C $(OBJCPP_MODELS_DIR) clean
endif
	$(QUIET)rm -rf $(CLANG_RESULTS_DIR) $(JAVA_RESULTS_DIR)
	$(QUIET)rm -rf $(JAVA_MODELS_OUT)
	$(QUIET)rm -f $(MODELS_JAR)
	$(QUIET)rm -f $(JAVA_MODELS_JAR)
