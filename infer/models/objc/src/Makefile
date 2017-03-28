# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

ROOT_DIR = ../../../..
include $(ROOT_DIR)/Makefile.config

M_SOURCES=$(shell find . -name "*.m")
C_SOURCES=$(shell find . -name "*.c")
OBJECTS=$(M_SOURCES:.m=.o) $(C_SOURCES:.c=.o)
CC=clang
XCODE_PATH=$(shell $(XCODE_SELECT) -p)
IPHONE_SIMULATOR_PATH=$(XCODE_PATH)/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk
CFLAGS=--target=x86_64-apple-darwin14 -x objective-c -c -mios-simulator-version-min=8.2 -isysroot $(IPHONE_SIMULATOR_PATH)

all: $(OBJECTS)

clean:
	$(QUIET)rm -rf $(OBJECTS)

%.o: %.m
	$(CC) $(CFLAGS) $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) $< -o $@

.PHONY: all clean
