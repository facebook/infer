# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

SOURCES = Resources.java Pointers.java Hello.java
OBJECTS = $(SOURCES:.java=.class)

all: Hello.class

%.class: $(SOURCES)
	javac $(SOURCES)

clean:
	rm -rf $(OBJECTS)
