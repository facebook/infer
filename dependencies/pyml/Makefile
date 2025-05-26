PREFIX := /usr/local
OCAMLFIND := ocamlfind
INSTALL := install
INSTALL_PROGRAM := $(INSTALL)
bindir := $(PREFIX)/bin

C_COMPILER := $(shell ocamlc -config | grep '^native_c_compiler:' | cut -d ' ' -f 2)
EXT_LIB := $(shell ocamlc -config | grep '^ext_lib:' | cut -d ' ' -f 2)

HAVE_OCAMLFIND := $(shell \
	if $(OCAMLFIND) query -help >/dev/null 2>&1; then \
		echo yes; \
	else \
		echo no; \
	fi \
)

HAVE_UTOP := $(shell \
	if [ "$(HAVE_OCAMLFIND)" = no ]; then \
		echo no; \
	elif $(OCAMLFIND) query utop >/dev/null 2>&1; then \
		echo yes; \
	else \
		echo no; \
	fi \
)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(HAVE_OCAMLFIND),no)
	OCAMLC := $(OCAMLFIND) ocamlc
	ifneq ($(HAVE_OCAMLOPT),no)
		OCAMLOPTEXE := $(OCAMLFIND) ocamlopt
	endif
	OCAMLMKLIB := $(OCAMLFIND) ocamlmklib
	OCAMLMKTOP := $(OCAMLFIND) ocamlmktop
	OCAMLDEP := $(OCAMLFIND) ocamldep
	OCAMLDOC := $(OCAMLFIND) ocamldoc
else
	OCAMLC := $(shell \
		if ocamlc.opt -version >/dev/null 2>&1; then \
			echo ocamlc.opt; \
		elif ocamlc -version >/dev/null 2>&1; then \
			echo ocamlc; \
		fi \
	)
	ifeq ($(OCAMLC),)
$(error There is no OCaml compiler available in path)
	endif
	ifneq ($(HAVE_OCAMLOPT),no)
		OCAMLOPTEXE := $(shell \
			if ocamlopt.opt -version >/dev/null 2>&1; then \
				echo ocamlopt.opt; \
			elif ocamlopt -version >/dev/null 2>&1; then \
				echo ocamlopt; \
			fi \
		)
	endif
	OCAMLMKLIB := ocamlmklib
	OCAMLMKTOP := ocamlmktop
	OCAMLDEP := ocamldep
	OCAMLDOC := ocamldoc
endif

OCAMLVERSION := $(shell $(OCAMLC) -version)
OCAMLVERSION_LIST := $(subst ., ,$(OCAMLVERSION))
OCAMLVERSION_MAJOR := $(word 1,$(OCAMLVERSION_LIST))

LIBRARIES := unix

ifeq ($(OCAMLVERSION_MAJOR),5)
	LIBRARIES_NUMPY = $(LIBRARIES)
	OCAMLCFLAGS = -I +unix
else
	LIBRARIES_NUMPY = $(LIBRARIES) bigarray
endif

null  :=
space := $(null) #
comma := ,

ifneq ($(HAVE_OCAMLFIND),no)
        OCAMLLDFLAGS += -linkpkg
	PACKAGES := $(subst $(space),$(comma),$(LIBRARIES))
	PACKAGES_NUMPY := $(subst $(space),$(comma),$(LIBRARIES_NUMPY))
	OCAMLBYTECODELIBS := -package $(PACKAGES)
	OCAMLBYTECODELIBSNUMPY := -package $(PACKAGES_NUMPY)
	OCAMLNATIVELIBS := -package $(PACKAGES)
	OCAMLNATIVELIBSNUMPY := -package $(PACKAGES_NUMPY)
else
	OCAMLBYTECODELIBS := $(LIBRARIES:=.cma)
	OCAMLBYTECODELIBSNUMPY := $(LIBRARIES_NUMPY:=.cma)
	OCAMLNATIVELIBS := $(LIBRARIES:=.cmxa)
	OCAMLNATIVELIBSNUMPY := $(LIBRARIES_NUMPY:=.cmxa)
endif

OCAMLVERSION := $(shell $(OCAMLC) -version)
endif

ifeq ($(HAVE_UTOP),yes)
	PYMLUTOP := pymlutop
else
	PYMLUTOP :=
endif

ifeq ($(OCAMLOPTEXE),)
	OCAMLOPT = $(error There is no optimizing OCaml compiler available)
	OCAMLCOPT := $(OCAMLC)
	CMOX := cmo
	CMAX := cma
	ALLOPT :=
	TESTOPT :=
	OCAMLPREFERREDLIBS := $(OCAMLBYTECODELIBS)
else
	OCAMLOPT := $(OCAMLOPTEXE)
	OCAMLCOPT := $(OCAMLOPT)
	CMOX := cmx
	CMAX := cmxa
	ALLOPT := all.native
	TESTOPT := test.native
	OCAMLPREFERREDLIBS := $(OCAMLNATIVELIBS)
endif

ifeq (4.06.0,$(word 1,$(sort 4.06.0 $(OCAMLVERSION))))
	PYOPS=pyops
else
	PYOPS=
endif

MODULES := pyml_arch pyutils pytypes pywrappers py pycaml $(PYOPS)

VERSION := $(shell date "+%Y%m%d")

OCAMLLIBFLAGS := -cclib "-L. -lpyml_stubs"
OCAMLLIBNUMPYFLAGS := -cclib "-L. -lnumpy_stubs"

OCAMLLIBFLAGSNATIVE := $(OCAMLLIBFLAGS)
OCAMLLIBFLAGSBYTECODE := -custom $(OCAMLLIBFLAGS)

INSTALL_FILES := \
	py.mli numpy.mli $(MODULES:=.cmi) $(MODULES:=.cmx) \
	numpy.cmi \
	pyml.cma pyml.cmxa pyml.cmxs pyml$(EXT_LIB) \
	numpy.cma numpy.cmxa numpy.cmxs numpy$(EXT_LIB) \
	$(MODULES:=.cmx) numpy.cmx \
	libpyml_stubs$(EXT_LIB) dllpyml_stubs.so \
	libnumpy_stubs$(EXT_LIB) dllnumpy_stubs.so \
	META

.PHONY : all
all : all.bytecode $(ALLOPT)
	@echo The py.ml library is compiled.
	@echo Run \`make doc\' to build the documentation.
	@echo Run \`make test\' to check the test suite.
ifneq ($(HAVE_OCAMLFIND),no)
	@echo Run \`make install\' to install the library via ocamlfind.
endif
	@echo Run \`make pymltop\' to build the toplevel.
ifneq ($(HAVE_UTOP),no)
	@echo Run \`make pymlutop\' to build the utop toplevel.
endif

.PHONY : help
help :
	@echo make [all] : build the library
	@echo make all.bytecode : build only the bytecode library
	@echo make all.native : build only the native library
	@echo make doc : build the documentation
ifneq ($(HAVE_OCAMLFIND),no)
	@echo make install : install the library via ocamlfind
endif
	@echo make clean : remove all the generated files
	@echo make tests : compile and run the test suite
	@echo make tests.bytecode : run only the bytecode version of the tests
	@echo make tests.native : run only the native version of the tests
	@echo make pymltop: build the toplevel
ifneq ($(HAVE_UTOP),no)
	@echo make pymlutop: build the utop toplevel.
endif
	@echo make HAVE_OCAMLFIND=no : disable ocamlfind
	@echo make HAVE_OCAMLOPT=no : disable ocamlopt
	@echo \
"make OCAMLC|OCAMLOPT|OCAMLMKLIB|OCAMLMKTOP|OCAMLDEP|OCAMLDOC=... :"
	@echo "  set paths to OCaml tools"
	@echo make OCAMLCFLAGS=... : set flags to OCaml compiler for compiling
	@echo make OCAMLLDFLAGS=... : set flags to OCaml compiler for linking
	@echo make OCAMLLIBFLAGS=... :
	@echo "  set flags to OCaml compiler for building the library"

.PHONY : all.bytecode
all.bytecode : pyml.cma numpy.cma

.PHONY : all.native
all.native : pyml.cmxa pyml.cmxs numpy.cmxa numpy.cmxs

.PHONY : test
test : test.bytecode $(TESTOPT)

.PHONY : test.bytecode
test.bytecode : pyml_tests.bytecode numpy_tests.bytecode
	./pyml_tests.bytecode
	./numpy_tests.bytecode

.PHONY : test.native
test.native : pyml_tests.native numpy_tests.native
	./pyml_tests.native
	./numpy_tests.native

.PHONY : install
install : $(INSTALL_FILES)
ifeq ($(HAVE_OCAMLFIND),no)
	$(error ocamlfind is needed for 'make install')
endif
	$(OCAMLFIND) install pyml $(INSTALL_FILES)
	[ ! -f pymltop ] || $(INSTALL_PROGRAM) pymltop $(bindir)/pymltop
	[ ! -f pymlutop ] || $(INSTALL_PROGRAM) pymlutop $(bindir)/pymlutop

.PHONY : uninstall
uninstall :
	$(OCAMLFIND) remove pyml
	- rm $(bindir)/pymltop
	- rm $(bindir)/pymlutop

.PHONY : clean
clean :
	for module in $(MODULES) numpy generate pyml_tests_common pyml_tests \
		numpy_tests; do \
		rm -f $$module.cmi $$module.cmo $$module.cmx $$module$(EXT_LIB) \
			$$module.o; \
	done
	rm -f pyml.cma pyml.cmxa pyml.cmxs pyml$(EXT_LIB)
	rm -f numpy.cma numpy.cmxa numpy.cmxs numpy$(EXT_LIB)
	rm -f pywrappers.mli pywrappers.ml pyml_dlsyms.inc pyml_wrappers.inc
	rm -f pyml.h
	rm -f pyml_stubs.o dllpyml_stubs.so libpyml_stubs$(EXT_LIB)
	rm -f numpy_stubs.o dllnumpy_stubs.so libnumpy_stubs$(EXT_LIB)
	rm -f pyml_arch.ml
	rm -f generate pyml_tests.native pyml_tests.bytecode
	rm -f numpy_tests.native numpy_tests.bytecode
	rm -f .depend
	rm -rf doc
	rm -f pymltop pytop.cmo pymlutop pyutop.cmo
	rm -f pymltop_libdir.ml pymltop_libdir.cmo
	rm -f pyops.mli pyops.ml

.PHONY : tarball
tarball :
	git archive --format=tar.gz --prefix=pyml-$(VERSION)/ HEAD \
		>pyml-$(VERSION).tar.gz

doc : py.mli pycaml.mli numpy.mli pywrappers.ml
	mkdir -p $@
	$(OCAMLDOC) $(OCAMLCFLAGS) -html -d $@ $^
	touch $@

.depend : $(MODULES:=.ml) $(MODULES:=.mli) numpy.ml numpy.mli \
	pyml_tests_common.mli pyml_tests_common.ml pyml_tests.ml numpy_tests.ml
	$(OCAMLDEP) $^ >$@

ifneq ($(MAKECMDGOALS),clean)
-include .depend
endif

pyutils.cmo pyutils.cmx : pyutils.cmi

generate : pyutils.$(CMOX) generate.$(CMOX)
	$(OCAMLCOPT) $(OCAMLLDFLAGS) $(OCAMLPREFERREDLIBS) $^ -o $@

generate.cmo : generate.ml

generate.cmx : generate.ml

pywrappers.ml pyml_wrappers.inc : generate
	./generate

pyml_wrappers.inc : pywrappers.ml

pywrappers.mli : pywrappers.ml pytypes.cmi pyml_arch.cmi
	$(OCAMLC) $(OCAMLCFLAGS) -i $< >$@

pyml_tests.native : py.cmi pyml.cmxa pyml_tests_common.cmx pyml_tests.cmx
	$(OCAMLOPT) $(OCAMLLDFLAGS) $(OCAMLNATIVELIBS) pyml.cmxa \
		pyml_tests_common.cmx pyml_tests.cmx -o $@

pyml_tests.bytecode : py.cmi pyml.cma pyml_tests_common.cmo pyml_tests.cmo
	$(OCAMLC) $(OCAMLLDFLAGS) $(OCAMLBYTECODELIBS) pyml.cma \
		pyml_tests_common.cmo pyml_tests.cmo -o $@

numpy_tests.native : py.cmi pyml.cmxa numpy.cmxa \
		pyml_tests_common.cmx numpy_tests.cmx
	$(OCAMLOPT) $(OCAMLLDFLAGS) $(OCAMLNATIVELIBSNUMPY) \
		pyml.cmxa numpy.cmxa \
		pyml_tests_common.cmx numpy_tests.cmx -o $@

numpy_tests.bytecode : py.cmi pyml.cma numpy.cma \
		pyml_tests_common.cmo numpy_tests.cmo
	$(OCAMLC) $(OCAMLLDFLAGS) $(OCAMLBYTECODELIBSNUMPY) pyml.cma \
		numpy.cma pyml_tests_common.cmo numpy_tests.cmo -o $@

pyml_arch.ml : pyml_arch.ml.c
	$(C_COMPILER) -E $< | sed '/^#/d' >$@

pyml_arch.cmo pyml_arch.cmx : pyml_arch.cmi

%.cmi : %.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c $< -o $@

%.cmo : %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $< -o $@

%.cmx : %.ml
	$(OCAMLOPT) $(OCAMLCFLAGS) -c $< -o $@

%.o : %.c
	$(OCAMLC) $(OCAMLCFLAGS) -c $< -o $@

pyml_stubs.o : pyml_wrappers.inc

pyml.cma : $(MODULES:=.cmo) libpyml_stubs$(EXT_LIB)
	$(OCAMLC) $(OCAMLLIBFLAGSBYTECODE) -a -dllib -lpyml_stubs $(MODULES:=.cmo) -o $@

pyml.cmxa : $(MODULES:=.cmx) libpyml_stubs$(EXT_LIB)
	$(OCAMLOPT) $(OCAMLLIBFLAGSNATIVE) -a $(MODULES:=.cmx) -o $@

pyml.cmxs : $(MODULES:=.cmx) libpyml_stubs$(EXT_LIB)
	$(OCAMLOPT) $(OCAMLLIBFLAGSNATIVE) -shared $(MODULES:=.cmx) -o $@

lib%$(EXT_LIB) : %.o
	$(OCAMLMKLIB) -o $(basename $<) $<

numpy.cma : numpy.cmo libnumpy_stubs$(EXT_LIB)
	$(OCAMLC) $(OCAMLLIBNUMPYFLAGS) -a -dllib -lnumpy_stubs numpy.cmo -o $@

numpy.cmxa : numpy.cmx libnumpy_stubs$(EXT_LIB)
	$(OCAMLOPT) $(OCAMLLIBNUMPYFLAGS) -a numpy.cmx -o $@

numpy.cmxs : numpy.cmx libnumpy_stubs$(EXT_LIB)
	$(OCAMLOPT) $(OCAMLLIBNUMPYFLAGS) -shared numpy.cmx -o $@

pytop.cmo : pytop.ml pymltop_libdir.cmi
	$(OCAMLC) -I +compiler-libs -c $<

pymltop_libdir.ml :
	if [ -z "$(PREFIX)" ]; then \
	  echo "let libdir=\"$(PWD)\""; \
	else \
	  echo "let libdir=\"$(PREFIX)/lib/pyml/\""; \
	fi >$@

pymltop : pyml.cma numpy.cma pymltop_libdir.cmo pytop.cmo
	$(OCAMLMKTOP) $(OCAMLLDFLAGS) $(OCAMLLIBNUMPYFLAGS) $(OCAMLBYTECODELIBSNUMPY) $^ -o $@

pyutop.cmo : pyutop.ml
ifeq ($(HAVE_OCAMLFIND),no)
	$(error ocamlfind is needed for utop)
endif
	$(OCAMLC) $(OCAMLCFLAGS) -thread -package utop -c $< -o $@

pymlutop : pyml.cma numpy.cma pymltop_libdir.cmo pytop.cmo pyutop.cmo
ifeq ($(HAVE_OCAMLFIND),no)
	$(error ocamlfind is needed for utop)
endif
#	ocamlmktop raises "Warning 31". See https://github.com/diml/utop/issues/212
#	$(OCAMLMKTOP) -o $@ -thread -linkpkg -package utop -dontlink compiler-libs $^
	ocamlfind ocamlc -thread -linkpkg -linkall -predicates create_toploop \
		-package compiler-libs.toplevel,utop $^ -o $@

pyops.ml: pyops.ml.new
	cp $< $@

pyops.mli: pyops.mli.new
	cp $< $@
