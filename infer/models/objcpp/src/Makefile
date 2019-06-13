# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

MODELS_DIR = ../..
include $(MODELS_DIR)/models-config.make

MM_SOURCES=$(wildcard *.mm)
M_SOURCES=$(shell find -L . -name "*.m")
C_SOURCES=$(shell find -L . -name "*.c")
OBJECTS=$(patsubst %.m,%_cxx.o, $(M_SOURCES)) $(patsubst %.c,%_cxx.o, $(C_SOURCES)) $(patsubst %.mm,%.o, $(MM_SOURCES))
# let infer override CC by letting the shell resolve its location according to PATH
CXX=clang++

IPHONESIMULATOR_ISYSROOT = $(XCODE_BASE)/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk
OBJC_TARGET = x86_64-apple-darwin14

CXXFLAGS += -Wno-deprecated-objc-isa-usage --target=$(OBJC_TARGET) -x objective-c++ -c -mios-simulator-version-min=8.2 -isysroot $(IPHONESIMULATOR_ISYSROOT)

all: $(OBJECTS)

clean:
	$(QUIET)rm -rf $(OBJECTS)

%.o: %.mm
	$(CXX) $(CXXFLAGS) $< -o $@

%_cxx.o: %.m
	$(CXX) $(CXXFLAGS) $< -o $@

%_cxx.o: %.c
	$(CXX) $(CXXFLAGS) $< -o $@

.PHONY: all clean
