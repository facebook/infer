0.8.5 2022-07-19
----------------

- Do not vendor `io.h` now that OCaml exports it (@dra27).

0.8.4 2022-05-10
----------------

- Upgrade Dune language version.

0.8.3 2021-11-12
----------------

- Improve the portability on Windows.

0.8.2 2019-12-20
----------------

- Do not check the terminal at startup for the Windows version — so it
  can be used in a continuous integration environment.

0.8.1 — 2018-09-27
------------------

- Port to Dune.
- Upgrade to OPAM 2.0.

0.8 — 2017-11-08
----------------

- Only use escape sequences or Windows calls when the stdout or stderr
  is a TTY. (issue #1).
- Port to jbuilder


