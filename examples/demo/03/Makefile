# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

SOURCES = Account.java AccountImplementation.java Client.java
OBJECT = Client.class

all: $(OBJECT)

$(OBJECT): $(SOURCES)
	javac $^

clean:
	rm -rf $(OBJECT)
