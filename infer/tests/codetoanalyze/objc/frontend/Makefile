# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


.PHONY: arc.clean
arc.clean:
	$(QUIET)$(MAKE) -C arc clean

.PHONY: noarc.clean
noarc.clean:
	$(QUIET)$(MAKE) -C noarc clean

.PHONY: arc.print
arc.print:
	$(QUIET)$(MAKE) -C arc print

.PHONY: noarc.print
noarc.print:
	$(QUIET)$(MAKE) -C noarc print

.PHONY: arc.test
arc.test:
	$(QUIET)$(MAKE) -C arc test

.PHONY: noarc.test
noarc.test:
	$(QUIET)$(MAKE) -C noarc test

.PHONY: arc.replace
arc.replace:
	$(QUIET)$(MAKE) -C arc replace

.PHONY: noarc.replace
noarc.replace:
	$(QUIET)$(MAKE) -C noarc replace

.PHONY: clean
clean: arc.clean noarc.clean

.PHONY: print
print: arc.print noarc.print

.PHONY: test
test: arc.test noarc.test

.PHONY: replace
replace: arc.replace noarc.replace
