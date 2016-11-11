# Copyright (c) 2015 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = ../../..
include $(ROOT_DIR)/Makefile.config

CPP_MODELS_FILE = $(SPECS_LIB_DIR)/cpp_models
CPP_MODELS_SOURCES = $(shell find src/ -name "*.cpp")
C_MODELS_SOURCES = $(shell find src/c_src/ -name "*.c")

all: $(CPP_MODELS_FILE)

$(CPP_MODELS_FILE): $(CPP_MODELS_SOURCES) $(C_MODELS_SOURCES) $(CLANG_DEPS)
# make clean in src/ in case $(CLANG_DEPS) have changed
	$(MAKE) -C src clean
	$(INFER_BIN) -o out/ --models-mode --no-failures-allowed --cxx -- $(MAKE) -C src
	touch $(CPP_MODELS_FILE)

install: $(CPP_MODELS_FILE)
	$(INSTALL_DATA) -C out/specs/*.specs $(SPECS_LIB_DIR)

clean:
	@rm -f $(CPP_MODELS_FILE)
	$(MAKE) -C src clean

.PHONY: all clean install
