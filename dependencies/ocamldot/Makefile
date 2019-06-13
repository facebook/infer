# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Try to automatically guess whether we are running under Windows.
# Set WIN32=true manually if this doesn't work.
#
ifeq (${OSTYPE},cygwin32) # Cygwin Beta 19
  WIN32=true
else
ifeq (${OSTYPE},cygwin)	  # Cygwin Beta 20
  WIN32=true
endif
endif

ifdef WIN32
EXE=.exe
else # UNIX
EXE=
endif

ocamldot$(EXE): ocamldot.mll
	ocamllex ocamldot.mll
	ocamlc -o $@ ocamldot.ml

clean:
	$(RM) ocamldot$(EXE) ocamldot.ml *.cmi *.cmo *.o *.obj
