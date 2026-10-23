![Continuous Integration](https://github.com/Chris00/ANSITerminal/actions/workflows/main.yml/badge.svg)

ANSITerminal
============

[ANSITerminal](src/ANSITerminal.mli) provides Basic control of ANSI
compliant terminals and the windows shell.

ANSITerminal is a module allowing to use the colors and cursor
movements on ANSI terminals. It also works on the windows shell (this
part is currently work in progress).

Install
-------

On Unix and OSX, the easier to install this library is to use
[opam](http://opam.ocaml.org/):

    opam install ANSITerminal
	
On Windows, until `opam` and `ocamlbuild` work well on this platform,
you clone this repository and compile and install this library by
executing:

    build

Documentation
-------------

The API may be [read online](http://chris00.github.io/ANSITerminal/doc/).
