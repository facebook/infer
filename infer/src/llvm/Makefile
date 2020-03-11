# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

ROOT_DIR = ../../..
include $(ROOT_DIR)/Makefile.config

INFER_BUILD_DIR = ../../_build/default

default: llvm_sil

$(BIN_DIR)/llvm_sil:
	rm -f $(INFER_BUILD_DIR)/llvm_sil.ml
	make -C .. infer
	cp llvm_sil.ml $(INFER_BUILD_DIR)
	cd $(INFER_BUILD_DIR) && \
	  ocamlfind ocamlopt -package llair -linkpkg llvm_sil.ml -o $(BIN_DIR)/llvm_sil
	rm -f $(INFER_BUILD_DIR)/llvm_sil.ml

.PHONY: llvm_sil
llvm_sil: $(BIN_DIR)/llvm_sil

.PHONY: clean
clean:
	rm -f $(INFER_BUILD_DIR)/llvm_sil.ml $(BIN_DIR)/llvm_sil
