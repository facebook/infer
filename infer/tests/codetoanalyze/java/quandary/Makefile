# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

include ../Makefile

ANALYZER = quandary
INFERPRINT_OPTIONS = --issues-txt

FILES = \
  Arrays.java \
  Basics.java \
  Fields.java \
  LoggingPrivateData.java \

compile:
	javac -cp $(CLASSPATH) $(FILES)

analyze:
	$(INFER_BIN) -a $(ANALYZER) -- javac -cp $(CLASSPATH) $(FILES) >/dev/null 2>/dev/null
