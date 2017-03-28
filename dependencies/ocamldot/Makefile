# Copyright (c) 2017 - present Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

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
