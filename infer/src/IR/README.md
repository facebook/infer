# Intermediate Representation

The Intermediate Representation is a format used by the back-end for analysis. It is produced by one of the front-ends, one for each program analyzed.

The main entry point is the intermediate language in [Sil](sil.mli).

The control flow graph module is [Cfg](cfg.mli).

The call graph module is [Cg](cg.mli).

The type environment module is [Tenv](tenv.mli).

