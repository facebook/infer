# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../..

PROJECT_ROOT = ../codetoanalyze/ndk-build/hello_app
SOURCES = $(PROJECT_ROOT)/hello.c
INFERPRINT_OPTIONS = --issues-tests
CLEAN_EXTRA = $(PROJECT_ROOT)/libs $(PROJECT_ROOT)/obj $(PROJECT_ROOT)/hello.o
NDKROOT = $(dir $(NDKBUILD))
NDKTOOLCHAIN = $(NDKROOT)/toolchains/llvm/prebuilt/linux-x86_64

include $(TESTS_DIR)/infer.make

infer-out/report.json: $(CLANG_DEPS) $(SOURCES)
	$(QUIET)cd $(PROJECT_ROOT) && \
	$(call silent_on_success,Testing ndk-build clang integration,\
	  $(INFER_BIN) --Xclang --rtlib=libgcc --results-dir $(CURDIR)/infer-out \
            --Xclang --sysroot=$(NDKTOOLCHAIN)/sysroot \
            --Xclang -L$(NDKTOOLCHAIN)/lib/gcc/i686-linux-android/4.9.x \
            --Xclang -L$(NDKTOOLCHAIN)/i686-linux-android/lib \
	    --Xclang -fuse-ld=bfd \
	    -- $(NDKBUILD) -B APP_ABI=x86 NDK_LIBS_OUT=./libs NDK_OUT=./obj V=1)
