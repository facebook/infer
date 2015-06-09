SHELL := /bin/bash
CWD = $(shell pwd)

ANNOTATIONS = annotations
MODELS = models
SRCDIR = src


.PHONY: clean clang java models

all: clang java

java:
	make -C $(SRCDIR) java
	make -C $(MODELS) java
	make -C $(ANNOTATIONS)

clang:
	make -C $(SRCDIR) clang
	make -C $(MODELS) clang

clean:
	make -C $(SRCDIR) clean
	make -C $(MODELS) clean
	make -C $(ANNOTATIONS) clean
