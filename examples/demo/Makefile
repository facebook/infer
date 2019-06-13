# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

SUBDIRS = 00 01 02 03 03.fixed 04 04.fixed

default: all

clean:
	$(foreach subdir,$(SUBDIRS),$(MAKE) -C $(subdir) clean;)
	rm -fr $(foreach subdir,$(SUBDIRS),$(subdir)/infer-out/)
	rm -fr infer-out/

all:
	$(foreach subdir,$(SUBDIRS),$(MAKE) -C $(subdir) all;)
