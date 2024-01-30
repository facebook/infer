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

RESULTS_DIR = infer-out
RESULTS_DB = $(RESULTS_DIR)/results.db
CAPTURE_DB = $(RESULTS_DIR)/capture.db

INFER_OPTIONS = --jobs 1 --biabduction-only --results-dir $(RESULTS_DIR) --biabduction-models-mode --scheduler=file

JAVA_MODELS_OUT = java/models
MODELS_JAR = models.jar

JAVA_SOURCES = $(shell find $(JAVA_BUILTINS_DIR) $(JAVA_MODELS_DIR) -name "*.java")
JAVA_MODELS_CLASSPATH = $(ANDROID_JAR):$(GUAVA_JAR):$(JACKSON_JAR):$(JSR_305_JAR):$(INFER_ANNOTATIONS_JAR)
JAVAC_OPTIONS = -bootclasspath $(ANDROID_JAR) -d $(JAVA_MODELS_OUT) -classpath $(JAVA_MODELS_CLASSPATH) \
	$(JAVA_SOURCES) -source 8 -target 8

SQL_DUMP_MODEL_SPECS = \
	-cmd ".mode insert model_specs" \
	-cmd ".output $(MODELS_RESULTS_FILE)" \
	-cmd "select * from specs order by proc_uid ;"

SQL_DUMP_MODEL_PROCS = \
	-cmd ".mode insert procedures" \
	-cmd "select * from procedures order by proc_uid ;"


.PHONY: all
all: $(MODELS_RESULTS_FILE)

capture:
ifeq ($(BUILD_C_ANALYZERS),yes)
	$(QUIET)$(call silent_on_success,Capturing C models,\
	  $(INFER_BIN) capture $(INFER_OPTIONS) --continue -- $(MAKE) -C $(C_MODELS_DIR) all)
	$(QUIET)$(call silent_on_success,Capturing C++ models,\
	  $(INFER_BIN) capture $(INFER_OPTIONS) --continue -- $(MAKE) -C $(CPP_MODELS_DIR) all)
ifeq (yes, $(HAS_OBJC))
	$(QUIET)$(call silent_on_success,Capturing ObjC models,\
	  $(INFER_BIN) capture $(INFER_OPTIONS) --continue -- $(MAKE) -C $(OBJC_MODELS_DIR) all)
endif
endif
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
	$(QUIET)$(REMOVE_DIR) $(JAVA_MODELS_OUT)
	$(QUIET)$(MKDIR_P) $(JAVA_MODELS_OUT)
	$(QUIET)$(REMOVE) $(JAVA_MODELS_JAR)
	$(QUIET)$(call silent_on_success,Capturing Java models,\
	  $(INFER_BIN) --continue capture $(INFER_OPTIONS) -- $(JAVAC) $(JAVAC_OPTIONS))
	cd $(JAVA_MODELS_OUT); jar cf ../../$(MODELS_JAR) *
#	install the models.jar file too
	$(INSTALL_DATA) -C $(MODELS_JAR) $(JAVA_MODELS_JAR)
	$(QUIET)touch $(JAVA_MODELS_JAR)
endif


# If the models deps have changed then the models need to be rebuilt. If infer itself has changed,
# we need to nuke the previous specs files in case the serialization has changed, otherwise we might
# encounter a segfault reading them.
$(MODELS_RESULTS_FILE): $(MAKEFILE_LIST)
#	clean first
	$(MAKE) clean
#	no parallelism between clang and java; racing on creating the db with --continue doesn't work
	$(MAKE) capture
	$(QUIET)$(call silent_on_success,Analyzing models,$(INFER_BIN) analyze $(INFER_OPTIONS))
	sqlite3 $(RESULTS_DB) $(SQL_DUMP_MODEL_SPECS) </dev/null
	sqlite3 $(CAPTURE_DB) $(SQL_DUMP_MODEL_PROCS) </dev/null >> $(MODELS_RESULTS_FILE)

ifeq ($(BUILD_C_ANALYZERS),yes)
$(MODELS_RESULTS_FILE): $(CLANG_DEPS_NO_MODELS)
endif
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
$(MODELS_RESULTS_FILE): $(JAVA_DEPS_NO_MODELS) $(JAVA_SOURCES)
endif

.PHONY: clean
clean:
	$(QUIET)$(REMOVE) $(MODELS_RESULTS_FILE)
	$(QUIET)$(REMOVE_DIR) $(RESULTS_DIR)
ifeq ($(BUILD_C_ANALYZERS),yes)
	$(QUIET)$(MAKE) -C $(C_MODELS_DIR) clean
	$(QUIET)$(MAKE) -C $(CPP_MODELS_DIR) clean
ifeq (yes, $(HAS_OBJC))
	$(QUIET)$(MAKE) -C $(OBJC_MODELS_DIR) clean
endif
endif
ifeq ($(BUILD_JAVA_ANALYZERS),yes)
	$(QUIET)$(REMOVE_DIR) $(JAVA_MODELS_OUT)
	$(QUIET)$(REMOVE) $(MODELS_JAR)
	$(QUIET)$(REMOVE) $(JAVA_MODELS_JAR)
endif
