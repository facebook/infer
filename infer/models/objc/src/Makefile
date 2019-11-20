# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

MODELS_DIR = ../..
include $(MODELS_DIR)/models-config.make

M_SOURCES=$(shell find . -name "*.m")
C_SOURCES=$(shell find . -name "*.c")
OBJECTS=$(M_SOURCES:.m=.o) $(C_SOURCES:.c=.o)
# let infer override CC by letting the shell resolve its location according to PATH
CC=clang

OBJC_TARGET = x86_64-apple-darwin14

CFLAGS += -Wno-deprecated-objc-isa-usage --target=$(OBJC_TARGET) -x objective-c -c -mios-simulator-version-min=8.2 -isysroot $(XCODE_ISYSROOT)

all: $(OBJECTS)

clean:
	$(QUIET)rm -rf $(OBJECTS)

%.o: %.m
	$(CC) $(CFLAGS) $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) $< -o $@

.PHONY: all clean
