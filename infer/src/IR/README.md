# Intermediate Representation

The Intermediate Representation is a format used by the back-end for analysis. It is produced by one of the front-ends, one for each program analyzed.

The main entry point is the intermediate language in [Sil](Sil.mli).

The control flow graph module is [Cfg](Cfg.mli).

The call graph module is [Cg](Cg.mli).

The type environment module is [Tenv](Tenv.mli).

