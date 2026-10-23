PREFIX_ARG := $(if $(PREFIX),--prefix $(PREFIX),)
LIBDIR_ARG := $(if $(LIBDIR),--libdir $(LIBDIR),)
DESTDIR_ARG := $(if $(DESTDIR),--destdir $(DESTDIR),)
INSTALL_ARGS := $(PREFIX_ARG) $(LIBDIR_ARG) $(DESTDIR_ARG)
DUNE_ARGS =
DUNE = dune

default:
	$(DUNE) build @install $(DUNE_ARGS)

install:
	$(DUNE) install $(INSTALL_ARGS) $(DUNE_ARGS)

test:
	$(DUNE) runtest $(DUNE_ARGS)

clean:
	$(DUNE) clean

doc:
	$(DUNE) build @doc $(DUNE_ARGS)
