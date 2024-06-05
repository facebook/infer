# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# NOTE: If you wish to deactivate a Topl test, simply remove it from this list
SUBDIRS = atom-literal atom-name fields file less process reach-fun reach-pair reach-simple specs str-literal taint taint-genserver track-xid

test-%:
	$(MAKE) -C $* test

test: $(SUBDIRS:%=test-%)

replace-%:
	$(MAKE) -C $* replace

replace: $(SUBDIRS:%=replace-%)

clean-%:
	$(MAKE) -C $* clean

clean: $(SUBDIRS:%=clean-%)
