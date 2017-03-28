# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

SOURCES=$(wildcard *.cpp)
CSOURCES=$(wildcard c_src/*.c)
OBJECTS=$(patsubst %.c,%_cxx.o, $(CSOURCES)) $(patsubst %.cpp,%.o, $(SOURCES))
CXX=clang++
CXXFLAGS=-c -w -std=c++11 -isystem../include

all: $(OBJECTS)

clean:
	$(QUIET)rm -rf $(OBJECTS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) $< -o $@

%_cxx.o: %.c
	$(CXX) $(CXXFLAGS) $< -o $@

.PHONY: all clean
