# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

SOURCES = $(shell ls *.c)
OBJECTS = $(SOURCES:.c=.o)

all: clean $(OBJECTS)
	echo $(OBJECTS)

.c.o:
	${CC} -c $<

clean:
	rm -rf $(OBJECTS)
