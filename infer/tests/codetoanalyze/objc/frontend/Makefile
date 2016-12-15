# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.


.PHONY: arc.clean noarc.clean
%.clean:
	@$(MAKE) -C $% clean

.PHONY: arc.print noarc.print
%.print:
	@$(MAKE) -C $% print

.PHONY: arc.test noarc.test
%.test:
	@$(MAKE) -C $% test

.PHONY: clean
clean: arc.clean noarc.clean

.PHONY: print
print: arc.print noarc.print

.PHONY: test
test: arc.test noarc.test
