# Copyright (c) 2019-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

SUBDIRS = 00 01 02 03

default: all

clean:
	$(foreach subdir,$(SUBDIRS),$(MAKE) -C $(subdir) clean;)
	rm -fr $(foreach subdir,$(SUBDIRS),$(subdir)/infer-out/)

all: $(OBJECTS)
