# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

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
