# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

include ../Makefile

ANALYZER = infer

FILES = \
  BasicHarnessActivity.java \

compile:
	javac -cp $(CLASSPATH) $(FILES)

analyze:
	$(call silent_on_success,$(INFER_BIN) -a $(ANALYZER) --android-harness -- javac -cp $(CLASSPATH) $(FILES))
