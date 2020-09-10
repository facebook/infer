# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

LEVEL=.
include Makefile.common

.PHONY: all
all:
	$(MAKE) -C libtooling/atdlib all
	$(MAKE) -C libtooling all

.PHONY: test
test: all
	$(MAKE) -C libtooling/atdlib test
	$(MAKE) -C libtooling test

.PHONY: clean
clean:
	$(MAKE) -C libtooling/atdlib clean
	$(MAKE) -C libtooling clean

.PHONY: fmt_all
fmt_all:
	find libtooling \
	  \( -name '*'.cpp -or -name '*'.h -or -name '*'.c -or -name '*'.m -or -name '*'.mm \) \
	  -exec	./clang/install/bin/clang-format -verbose -i \{\} \+
