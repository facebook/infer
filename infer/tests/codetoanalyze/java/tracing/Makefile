# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

include ../Makefile

ANALYZER = tracing

FILES_FOR_TRACING = \
  T2.java \
  \
  ArrayIndexOutOfBoundsExceptionExample.java \
  ClassCastExceptionExample.java \
  LazyDynamicDispatchExample.java \
  LocallyDefinedExceptionExample.java \
  NullPointerExceptionExample.java \
  ReportOnMainExample.java \
  UnavoidableExceptionExample.java \
  \

FILES_LINKED_FROM_INFER = \
  T.java \
  SkippedSourceFile.java \
  SomeLibrary.java \
  Utils.java \
  CloseableAsResourceExample.java \
  NeverNullSource.java \
  \
  ArrayOutOfBounds.java \
  ClassCastExceptions.java \
  NullPointerExceptions.java \

FILES = $(FILES_FOR_TRACING) $(FILES_LINKED_FROM_INFER)

compile:
	javac -cp $(CLASSPATH) $(FILES)

analyze:
	$(INFER_BIN) -a $(ANALYZER) -- javac -cp $(CLASSPATH) $(FILES) >/dev/null 2>/dev/null
