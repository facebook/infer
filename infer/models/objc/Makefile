SHELL := /bin/bash
CWD = $(shell pwd)
BINDIR = $(CWD)/../../bin
LIBDIR = $(CWD)/../../lib
LIB_SPECS = $(LIBDIR)/specs
OBJC_MODELS_FILE = $(LIB_SPECS)/objc_models

INFER = ANALYZE_MODELS=1 $(BINDIR)/infer

default: run_infer

.PHONY: run_infer install clean

run_infer: clean
	$(INFER) -o $(CWD)/out --models_mode --no_failures_allowed -- make -C src -j

install: run_infer
	cp out/specs/*.specs $(LIB_SPECS)
	touch $(OBJC_MODELS_FILE)
	rm -rf out

clean:
	if [ -a $(OBJC_MODELS_FILE) ];then rm $(OBJC_MODELS_FILE);fi
	make -C src clean
