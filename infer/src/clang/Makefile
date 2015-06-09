
REMOVE = rm -vf

EXAMPLE_SOURCES = test.c
EXAMPLE_AST = test.ast

.PHONY: lib

all: $(EXAMPLE_AST)

lib:
	ocamlbuild -I backend cMain.cma

$(EXAMPLE_AST): $(EXAMPLE_SOURCES)
	clang -cc1 -ast-dump test.c > test.ast

clean:
	ocamlbuild -clean
	$(REMOVE) *~ $(EXAMPLE_AST)
