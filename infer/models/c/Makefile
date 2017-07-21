# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = ../../..
include $(ROOT_DIR)/Makefile.config

C_MODELS_SOURCES = $(shell find src/ -name "*.[ch]")
INFER_RESULTS = out/report.json

all: install

$(INFER_RESULTS): $(C_MODELS_SOURCES) $(CLANG_DEPS_NO_MODELS)
#	make clean in src/ to recompile all the models
	$(QUIET)$(call silent_on_success,Building C models,\
	  $(INFER_BIN) -o $(@D) --models-mode -- $(MAKE) -C src clean all)

$(C_MODELS_FILE): $(INFER_RESULTS)
	$(QUIET)$(INSTALL_DATA) $(dir $(INFER_RESULTS))/specs/*.specs $(SPECS_LIB_DIR)
	$(QUIET)touch $@

install: $(C_MODELS_FILE)

clean:
	$(REMOVE) $(C_MODELS_FILE)
	$(REMOVE_DIR) $(dir $(INFER_RESULTS))
	$(QUIET)$(MAKE) -C src clean

.PHONY: all clean install
