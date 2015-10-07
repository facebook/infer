SHELL := /bin/bash
CWD = $(shell pwd)

ANNOTATIONS = annotations
MODELS = models
SRCDIR = src


.PHONY: clean clang java llvm checkCopyright models

all: clang java llvm checkCopyright

java:
	make -C $(SRCDIR) java
	make -C $(ANNOTATIONS)
	make -C $(MODELS) java

clang:
	make -C $(SRCDIR) clang
	make -C $(MODELS) clang

llvm:
	make -C $(SRCDIR) llvm

checkCopyright:
	make -C $(SRCDIR) checkCopyright

clean:
	make -C $(SRCDIR) clean
	make -C $(ANNOTATIONS) clean
	make -C $(MODELS) clean
