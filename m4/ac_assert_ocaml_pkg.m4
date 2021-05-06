dnl Copyright (c) Facebook, Inc. and its affiliates.
dnl
dnl This source code is licensed under the MIT license found in the
dnl LICENSE file in the root directory of this source tree.

dnl AC_ASSERT_OCAML_PKG([pkg_name], [pkg_version])
dnl
dnl fails if $pkg_name is not installed; also fails if it is not
dnl installed at version $pkg_version if specified
dnl
dnl assumes that AC_PROG_FINDLIB has been called
AC_DEFUN([AC_ASSERT_OCAML_PKG],
[dnl
  AC_CHECK_OCAML_PKG_PATH([$1], [$2], [$3])

  unset has_pkg
  unset pkg
  unset version

  AS_IF([test "$OCAMLFIND" = "no"], [dnl
    # trick to detect if ocamlfind is correctly installed and give the
    # right opam instructions to install it if not since they're the
    # same as when any other opam package is missing.
    has_pkg=no
    pkg=ocamlfind
    version=
  ], [dnl
    has_pkg=$AS_TR_SH[OCAML_PKG_$1]
    pkg=$1
    version=
    AS_IF([test "x$3" != "x"], [dnl
      version=" version $3"
    ])
  ])
  AS_IF([test "$has_pkg" = "no"], [dnl
    AC_MSG_ERROR([missing OCaml dependency: $pkg$version

If you are using opam, please run

  opam update
  opam install --deps-only opam/infer.opam])
  ])
])
