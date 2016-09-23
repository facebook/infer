# Copyright (c) 2016 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

include ../Makefile


ANALYZER = crashcontext

FILES = \
  BranchingCallsExample \
  MethodNameClashExample \
  MinimalCrashExample \
  MultiStackFrameCrashExample \
  NativeMethodExample \

compile:
	for file in $(FILES) ; do \
	  javac -cp $(CLASSPATH) $$file.java ; \
	done

analyze:
	for file in $(FILES) ; do \
	  $(INFER_BIN) -a $(ANALYZER) --stacktrace $$file.stacktrace.json -- javac -cp $(CLASSPATH) $$file.java >/dev/null 2>/dev/null ; \
	  cp infer-out/crashcontext/crashcontext.json $$file.exp.test ; \
	done

# overwrite
print:
	
# overwrite
test: analyze
	set -e; \
	for file in $(FILES) ; do \
    diff -u $$file.exp $$file.exp.test ; \
    rm $$file.exp.test ; \
	done  
	make clean

