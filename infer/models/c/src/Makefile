# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

SOURCES=$(wildcard *.c)
OBJECTS=$(SOURCES:.c=.o)
CC=clang
CFLAGS=-c -w

all: $(OBJECTS) 

clean:
	$(QUIET)rm -rf $(OBJECTS) 

%.o: %.c
	$(CC) $(CFLAGS) $< -o $@

.PHONY: all clean
