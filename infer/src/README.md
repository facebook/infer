The OCaml source files for infer live here. The Makefile is
responsible for building them, together with jbuild.in.

If you make changes to the build process, also update .merlin
accordingly. For instance, if you want to add a new source directory
you will need to:
- add it to jbuild.in
- add it to .merlin
