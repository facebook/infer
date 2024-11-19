mypkgdir=$(pkglibdir)

EXTRA_DIST= stdcompat.opam dune stdcompat.ml stdcompat__native.ml_byte \
	stdcompat__native.ml_native stdcompat_tests.ml stdcompat__native.mli \
	stdcompat__pervasives.mli stdcompat__arg.mli stdcompat__lazy.mli \
	stdcompat__char.mli stdcompat__uchar.mli stdcompat__sys.mli stdcompat__buffer.mli \
	stdcompat__string.mli stdcompat__stringLabels.mli stdcompat__bytes.mli \
	stdcompat__bytesLabels.mli stdcompat__list.mli \
	stdcompat__listLabels.mli \
	stdcompat__stack.mli \
	stdcompat__hashtbl.mli stdcompat__set.mli \
	stdcompat__map.mli stdcompat__weak.mli \
	stdcompat__digest.mli stdcompat__nativeint.mli \
	stdcompat__int.mli \
	stdcompat__int64.mli stdcompat__int32.mli stdcompat__filename.mli \
	stdcompat__array.mli stdcompat__arrayLabels.mli \
	stdcompat__float.mli stdcompat__queue.mli stdcompat__ephemeron.mli \
	stdcompat__moreLabels.mli stdcompat__lexing.mli \
	stdcompat__option.mli stdcompat__printexc.mli \
	stdcompat__random.mli \
	stdcompat__result.mli stdcompat__bool.mli stdcompat__domain.mli \
	stdcompat__fun.mli stdcompat__format.mli stdcompat__printf.mli stdcompat__stdlib.mli \
	stdcompat__seq.mli stdcompat__printexc.mli \
        stdcompat__either.mli stdcompat__atomic.mli \
	stdcompat__unit.mli \
	stdcompat__in_channel.mli stdcompat__out_channel.mli Makefile.bootstrap VERSION \
	dune-project dune

MODULES = stdcompat__init.ml stdcompat__root.ml \
	stdcompat__either_s.ml stdcompat__either.ml \
	stdcompat__seq_s.ml \
	stdcompat__seq.ml stdcompat__tools.ml \
	stdcompat__pervasives_s.ml stdcompat__arg_s.ml stdcompat__lazy_s.ml \
	stdcompat__char_s.ml stdcompat__uchar_s.ml stdcompat__uchar.ml \
        stdcompat__buffer_s.ml \
	stdcompat__string_s.ml stdcompat__stringLabels_s.ml stdcompat__bytes_s.ml \
	stdcompat__bytesLabels_s.ml \
	stdcompat__list_s.ml \
        stdcompat__listLabels_s.ml \
	stdcompat__stack_s.ml \
	stdcompat__list.ml \
	stdcompat__hashtbl_ext.ml stdcompat__hashtbl_s.ml stdcompat__set_s.ml \
	stdcompat__map_s.ml stdcompat__weak_s.ml stdcompat__sys_s.ml \
	stdcompat__digest_s.ml stdcompat__nativeint_s.ml \
	stdcompat__int_s.ml \
	stdcompat__int64_s.ml stdcompat__int32_s.ml stdcompat__filename_s.ml \
	stdcompat__array_s.ml stdcompat__arrayLabels_s.ml \
	stdcompat__float_s.ml stdcompat__queue_s.ml \
	stdcompat__hashtbl.ml \
	stdcompat__ephemeron_s.ml \
	stdcompat__moreLabels_s.ml stdcompat__lexing_s.ml \
	stdcompat__pervasives.ml \
	stdcompat__option_s.ml stdcompat__result_s.ml stdcompat__bool_s.ml \
	stdcompat__fun_s.ml stdcompat__printf_s.ml stdcompat__domain_s.ml \
	stdcompat__domain.ml \
	stdcompat__printexc_s.ml stdcompat__format_s.ml stdcompat__atomic_s.ml \
	stdcompat__in_channel_s.ml stdcompat__out_channel_s.ml \
	stdcompat__random_s.ml \
	stdcompat__unit_s.ml \
	stdcompat__stdlib_s.ml \
	stdcompat__arg.ml stdcompat__lazy.ml \
	stdcompat__char.ml \
	stdcompat__option.ml \
	stdcompat__buffer.ml stdcompat__sys.ml \
	stdcompat__bytes.ml stdcompat__bytesLabels.ml \
	stdcompat__string.ml stdcompat__stringLabels.ml \
	stdcompat__filename.ml \
        stdcompat__listLabels.ml \
	stdcompat__stack.ml stdcompat__set.ml \
	stdcompat__map.ml stdcompat__weak.ml \
	stdcompat__random.ml \
	stdcompat__digest.ml stdcompat__nativeint.ml \
	stdcompat__int.ml stdcompat__int64.ml stdcompat__int32.ml \
	stdcompat__array.ml stdcompat__arrayLabels.ml \
	stdcompat__float.ml stdcompat__queue.ml stdcompat__ephemeron.ml \
	stdcompat__moreLabels.ml stdcompat__lexing.ml \
	stdcompat__printexc.ml stdcompat__result.ml stdcompat__bool.ml \
	stdcompat__fun.ml stdcompat__format.ml stdcompat__printf.ml \
	stdcompat__atomic.ml \
	stdcompat__in_channel.ml stdcompat__out_channel.ml \
	stdcompat__unit.ml \
	stdcompat__stdlib.ml stdcompat.ml

# From https://github.com/fdopen/opam-repository-mingw/commit/3b4c52e17cfae96cfbf5120b60b8c2bbf5aad252
# We prefer DLLEXT to conform with EXEEXT and OBJEXT
DLLEXT := $(shell ocamlc -config | awk -F '[\t\r ]+' '/^ext_dll:/ {print $$2}')

# Fix install on Windows (https://github.com/thierry-martinez/pyml/issues/88)
LIBEXT := $(shell ocamlc -config | awk -F '[\t\r ]+' '/^ext_lib:/ {print $$2}')

MODULES_native = stdcompat__native.ml $(MODULES)

mypkg_SCRIPTS = META $(MODULES_native) $(MODULES_native:.ml=.cmi) \
	libstdcompat__stubs$(LIBEXT) stdcompat.h

if OCAML_SUPPORTS_BYTECODE
mypkg_SCRIPTS += stdcompat.cma
BEST_SUFFIX := .cmo
endif

if OCAML_SUPPORTS_NATIVE
mypkg_SCRIPTS += stdcompat.cmxa stdcompat$(LIBEXT) $(MODULES_native:.ml=.cmx)
BEST_SUFFIX := .cmx
endif

if OCAML_SUPPORTS_SHARED
mypkg_SCRIPTS += stdcompat.cmxs
endif

PACKAGES = $(RESULT_PKG) $(SEQ_PKG) $(UCHAR_PKG)
OCAMLFLAGS = $(PACKAGES:%=-package %)
OCAMLCFLAGS = $(OCAMLFLAGS) -nolabels -I @srcdir@
OCAMLLDFLAGS = $(OCAMLFLAGS)
OCAMLLIBFLAGS = -cclib "-L. -lstdcompat__stubs"

if OCAML_SUPPORTS_BIN_ANNOT
OCAMLFLAGS += -bin-annot
mypkg_SCRIPTS += $(MODULES_native:.ml=.cmt)
endif

if OCAML_SUPPORTS_NO_ALIAS_DEPS
OCAMLFLAGS += -no-alias-deps
endif

if OCAML_SUPPORTS_ALERT
OCAMLCFLAGS += -alert -deprecated
endif

STDCOMPAT_TESTS_EXE = stdcompat_tests$(EXEEXT)

SUFFIXES = .ml .mli .cmi .cmo .cmx .cmxa .cmxs .cmt

.DEFAULT_GOAL = all

.mli.cmi :
	$(OCAMLBEST) -c $(OCAMLCFLAGS) $< -o $@

.ml.cmo :
	$(OCAMLC) -c $(OCAMLCFLAGS) $< -o $@

.ml.cmx :
	$(OCAMLOPT) -c $(OCAMLCFLAGS) $< -o $@

$(BEST_SUFFIX).cmt : ;

stdcompat__stubs.$(OBJEXT) : stdcompat__stubs.c
	$(OCAMLC) -c $<

libstdcompat__stubs$(LIBEXT) : stdcompat__stubs.$(OBJEXT)
	$(OCAMLMKLIB) -o stdcompat__stubs $<

dllstdcompat__stubs$(DLLEXT) : libstdcompat__stubs$(LIBEXT)

stdcompat.cma : $(MODULES_native:.ml=.cmo) libstdcompat__stubs$(LIBEXT)
	$(OCAMLC) $(OCAMLLDFLAGS) -a $(MODULES_native:.ml=.cmo) \
	  $(OCAMLLIBFLAGS) -dllib -lstdcompat__stubs -o $@

stdcompat.cmxa : $(MODULES_native:.ml=.cmx) libstdcompat__stubs$(LIBEXT)
	$(OCAMLOPT) $(OCAMLLDFLAGS) -a $(MODULES_native:.ml=.cmx) $(OCAMLLIBFLAGS) -o $@

stdcompat$(LIBEXT) : stdcompat.cmxa

stdcompat.cmxs : $(MODULES_native:.ml=.cmx) libstdcompat__stubs$(LIBEXT)
	$(OCAMLOPT) $(OCAMLLDFLAGS) -shared $(MODULES_native:.ml=.cmx) $(OCAMLLIBFLAGS) -o $@

# Fake dependency to rebuild stdcompat__native.cmi on compiler change
stdcompat__native.cmi : config.status

stdcompat__native.cmo : stdcompat__native.ml_byte stdcompat__native.cmi
	$(OCAMLC) $(OCAMLCFLAGS) -c -impl stdcompat__native.ml_byte

stdcompat__native.cmx : stdcompat__native.ml_native stdcompat__native.cmi
	$(OCAMLOPT) $(OCAMLCFLAGS) -c -impl stdcompat__native.ml_native

.PHONY : depend
depend : .depend

.depend : $(MODULES) $(MODULES_native:.ml=.mli) stdcompat.ml stdcompat__native.ml_byte
	cp stdcompat__native.ml_byte stdcompat__native.ml
	$(OCAMLDEP) $^ >$@ || rm -f .depend

.PHONY : test
test : $(STDCOMPAT_TESTS_EXE)
	./$(STDCOMPAT_TESTS_EXE)

$(STDCOMPAT_TESTS_EXE): stdcompat.$(CMAX) stdcompat_tests.ml
	$(OCAMLBEST) stdcompat.$(CMAX) stdcompat_tests.ml -o $@

@INCLUDE_DEPEND@

CLEANFILES = *.cmi *.cmo *.cmx *.cmt *.cmti *.$(OBJEXT) *$(LIBEXT) stdcompat.cma \
	stdcompat.cmxa stdcompat__native.ml dllstdcompat__stubs$(DLLEXT) \
	stdcompat.cmxs $(STDCOMPAT_TESTS_EXE) $(bin_SCRIPTS) .depend

install-exec-hook :
	$(MKDIR_P) $(DESTDIR)$(libdir)/stublibs
	$(INSTALL_PROGRAM) dllstdcompat__stubs$(DLLEXT) $(DESTDIR)$(libdir)/stublibs
	echo stdcompat >$(DESTDIR)$(libdir)/stublibs/dllstdcompat__stubs$(DLLEXT).owner
