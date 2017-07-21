# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

C_SOURCES = $(shell ls *.c)
JAVA_SOURCES = Resources.java Pointers.java Hello.java
SOURCES = $(C_SOURCES) $(JAVA_SOURCES)
C_OBJECTS = $(C_SOURCES:.c=.o)
JAVA_OBJECTS = $(JAVA_SOURCES:.java=.class)
OBJECTS = $(C_OBJECTS) $(JAVA_OBJECTS)

all: $(C_OBJECTS) Hello.class

.c.o:
	$(CC) -c $<

%.class: $(JAVA_SOURCES)
	javac $(JAVA_SOURCES)

clean:
	rm -rf $(OBJECTS)
