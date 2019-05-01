# Copyright (c) 2017-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

SOURCES = simple_null.cpp
OBJECTS = $(SOURCES:.cpp=.o)

all: $(OBJECTS)

.cpp.o:
	${CC} -c $<

clean:
	rm -rf $(OBJECTS)
