# Copyright (c) 2017-present, Facebook, Inc.
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
XCODE_PATH=$(shell $(XCODE_SELECT) -p)
IPHONE_SIMULATOR_PATH=$(XCODE_PATH)/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk
CXXFLAGS += -Wno-deprecated-objc-isa-usage --target=x86_64-apple-darwin14 -x objective-c++ -c -mios-simulator-version-min=8.2 -isysroot $(IPHONE_SIMULATOR_PATH)

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
