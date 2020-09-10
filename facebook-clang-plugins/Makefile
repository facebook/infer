# Copyright (c) 2014-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

LEVEL=.
include Makefile.common

.PHONY: all
all:
	$(MAKE) -C libtooling/atdlib all
	$(MAKE) -C libtooling all

.PHONY: test
test: all
	$(MAKE) -C libtooling/atdlib test
	$(MAKE) -C libtooling test

.PHONY: clean
clean:
	$(MAKE) -C libtooling/atdlib clean
	$(MAKE) -C libtooling clean

.PHONY: fmt_all
fmt_all:
	find libtooling \
	  \( -name '*'.cpp -or -name '*'.h -or -name '*'.c -or -name '*'.m -or -name '*'.mm \) \
	  -exec	./clang/install/bin/clang-format -verbose -i \{\} \+

CHECKCOPYRIGHT=../infer/bin/checkCopyright

.PHONY: copyright
copyright:
	@[ -x $(CHECKCOPYRIGHT) ] || { \
	  echo "only works when this repo is checked out as a git submodule inside the infer repo" >&2; \
	  echo "make sure this is the case and that `make checkCopyright` has been run from within infer" >&2; \
	  exit 1; \
	}
	git ls-files \
	| grep -e '\(\.\(atd\|c\|cpp\|h\|m\|ml\|mli\|mm\|py\|sh\)\(\|\.p\)$$\|^\(.*/\|\)Makefile\)' \
	| grep -v 'libtooling/ASTExporter\.\(cpp\|h\)' \
	| xargs $(CHECKCOPYRIGHT) -i
