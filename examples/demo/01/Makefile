# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

SOURCES = simple_null_interproc.c wrappers.c
OBJECTS = $(SOURCES:.c=.o)

all: $(OBJECTS)

.cpp.o:
	${CC} -c $<

clean:
	rm -rf $(OBJECTS)
