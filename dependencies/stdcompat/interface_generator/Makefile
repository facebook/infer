OCAMLFIND := ocamlfind

OCAMLFIND_AVAILABLE := $(shell \
	if $(OCAMLFIND) query -help >/dev/null 2>&1; then \
		echo true; \
	else \
		echo false; \
	fi \
)

ifeq ($(OCAMLFIND_AVAILABLE),true)
	OCAMLOPT := $(OCAMLFIND) ocamlopt
	OCAMLC := $(OCAMLFIND) ocamlc
	OCAMLDOC := $(OCAMLFIND) ocamldoc
	OCAMLDEP := $(OCAMLFIND) ocamldep
	OCAMLFLAGS := -package unix,compiler-libs.common
	OCAMLLDFLAGS := -linkpkg
else
	OCAMLOPT := $(shell \
		if ocamlopt.opt -version >/dev/null 2>&1; then \
			echo ocamlopt.opt; \
		elif ocamlopt -version >/dev/null 2>&1; then \
			echo ocamlopt; \
		fi \
	)

	OCAMLC := $(shell \
		if ocamlc.opt -version >/dev/null 2>&1; then \
			echo ocamlc.opt; \
		elif ocamlc -version >/dev/null 2>&1; then \
			echo ocamlc; \
		fi \
	)

	OCAMLDOC := ocamldoc

	OCAMLDEP := ocamldep
endif

OCAMLFLAGS += -g
OCAMLCFLAGS += $(OCAMLFLAGS)
OCAMLLDFLAGS += $(OCAMLFLAGS)

ifeq ($(OCAMLC),)
$(error There is no OCaml compiler available in PATH)
endif

ifeq ($(OCAMLOPT),)
	OCAMLOPT_AVAILABLE := false
else
	OCAMLOPT_AVAILABLE := $(shell \
		if $(OCAMLOPT) -version >/dev/null 2>&1; then \
			echo true; \
		else \
			echo false; \
		fi \
	)
endif

ifeq ($(OCAMLOPT_AVAILABLE),true)
	OCAMLBEST := $(OCAMLOPT)
	CMOX := cmx
else
	OCAMLBEST := $(OCAMLC)
	CMOX := cmo
endif

MODULES := interface_tools.ml interface_dumper.ml interface_generator.ml

all : interface_dumper interface_generator

clean :
	- rm -f $(MODULES:.ml=.cmo) $(MODULES:.ml=.cmx) $(MODULES:.ml=.cmi) \
		$(MODULES:.ml=.o) .depend

.depend : $(MODULES) $(MODULES:.ml=.mli)
	$(OCAMLDEP) $^ >$@

ifneq ($(MAKECMDGOALS),clean)
-include .depend
endif

interface_dumper : interface_tools.$(CMOX) interface_dumper.$(CMOX)
	$(OCAMLBEST) $(OCAMLLDFLAGS) $^ -o $@

interface_generator : interface_tools.$(CMOX) interface_generator.$(CMOX)
	$(OCAMLBEST) $(OCAMLLDFLAGS) $^ -o $@

%.cmi : %.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c $< -o $@

%.cmo : %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $< -o $@

%.cmx : %.ml
	$(OCAMLOPT) $(OCAMLCFLAGS) -c $< -o $@
