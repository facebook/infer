# The CamlZip library

## DESCRIPTION

This Objective Caml library provides easy access to compressed files in ZIP and GZIP format, as well as to Java JAR files.  It provides functions for reading from and writing to compressed files in these formats.

## REQUIREMENTS

* Objective Caml 4.07 or up.

* The Findlib / ocamlfind library manager.

* The Zlib C library, version 1.1.3 or up.  You need both the library and its development headers.  For Debian and Ubuntu, install the package `zlib1-dev`.  For Fedora and RHEL, install the package `zlib-devel`.  The Zlib source distribution is at https://zlib.net/ .

## INSTALLATION

* Do `make all`.

* If it complains that `zlib.h` or `-lz` cannot be found, it is probably because Zlib is installed in non-standard directories.  Edit the top of the Makefile to set the appropriate values for `ZLIB_LIBDIR` and `ZLIB_INCLUDE`, or pass these values to `make`, for example:
```
        make ZLIB_LIBDIR=/opt/lib ZLIB_INCLUDE=/opt/include all
```

* Become super-user if necessary and do `make install`.  This installs the library through ocamlfind.

## DOCUMENTATION AND USAGE

See the comments in files zip.mli and gzip.mli.  Alternatively, do `make doc` and open the file `./doc/index.html`.

Compilation:      `ocamlfind ocamlopt -package zip ...`
Linking:          `ocamlfind ocamlopt -package zip -linkpgk ...`

The directory test/ contains examples of using this library.

## LICENSING

This library is copyright 2001, 2002, 2006, 2007, 2008, 2016, 2017, 2020 Institut National de Recherche en Informatique et en Automatique (INRIA), and distributed under the terms of the GNU Lesser General Public License (LGPL) version 2.1 or above, with a special exception concerning static linking.  See the file LICENSE for the exact licensing terms.

## BUG REPORTS AND USER FEEDBACK

Please use the [issue tracker](https://github.com/xavierleroy/camlzip/issues)

