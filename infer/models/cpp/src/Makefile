# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

SOURCES=$(wildcard *.cpp)
CSOURCES=$(wildcard c_src/*.c)
OBJECTS=$(patsubst %.c,%_cxx.o, $(CSOURCES)) $(patsubst %.cpp,%.o, $(SOURCES))
CXX=clang++
CXXFLAGS=-c -w -std=c++11

all: $(OBJECTS)

clean:
	$(QUIET)rm -rf $(OBJECTS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) $< -o $@

%_cxx.o: %.c
	$(CXX) $(CXXFLAGS) $< -o $@

.PHONY: all clean
