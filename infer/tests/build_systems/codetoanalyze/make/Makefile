# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

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
