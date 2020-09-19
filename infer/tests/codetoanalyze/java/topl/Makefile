# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

TESTS_DIR = ../../..

SUBDIRS = baos compareArgs hasnext skip servlet slowIter

test-%:
	$(MAKE) -C $* test

test: $(SUBDIRS:%=test-%)

replace-%:
	$(MAKE) -C $* replace

replace: $(SUBDIRS:%=replace-%)

clean-%:
	$(MAKE) -C $* clean

clean: $(SUBDIRS:%=clean-%)
