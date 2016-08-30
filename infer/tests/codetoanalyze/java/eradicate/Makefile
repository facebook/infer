# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

include ../Makefile

ANALYZER = eradicate

FILES = \
  ActivityFieldNotInitialized.java \
  FieldNotInitialized.java \
  FieldNotNullable.java \
  InconsistentSubclassAnnotationInterface.java \
  InconsistentSubclassAnnotation.java \
  LibraryCalls.java \
  NoReuseUndefFunctionValues.java \
  NullFieldAccess.java \
  NullMethodCall.java \
  ParameterNotNullable.java \
  ReturnNotNullable.java \
  SuppressWarningsExample.java \
  SuppressedFieldNotInitializedExample.java

compile:
	javac -cp $(CLASSPATH) $(FILES)

analyze:
	ERADICATE_RETURN_OVER_ANNOTATED=1 \
	$(INFER_BIN) -a $(ANALYZER) -- javac -cp $(CLASSPATH) $(FILES) >/dev/null 2>/dev/null
