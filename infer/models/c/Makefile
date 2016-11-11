# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = ../../..
include $(ROOT_DIR)/Makefile.config

C_MODELS_FILE = $(SPECS_LIB_DIR)/c_models
C_MODELS_SOURCES = $(shell find src/ -name "*.c")

all: $(C_MODELS_FILE)

$(C_MODELS_FILE): $(C_MODELS_SOURCES) $(CLANG_DEPS)
# make clean in src/ in case $(CLANG_DEPS) have changed
	$(MAKE) -C src clean
	$(INFER_BIN) -o out/ --models-mode --no-failures-allowed -- $(MAKE) -C src
	touch $(C_MODELS_FILE)

install: $(C_MODELS_FILE)
	$(INSTALL_DATA) -C out/specs/*.specs $(SPECS_LIB_DIR)

clean:
	@rm -f $(C_MODELS_FILE)
	$(MAKE) -C src clean

.PHONY: all clean install
