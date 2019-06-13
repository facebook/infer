dnl Copyright (c) Facebook, Inc. and its affiliates.
dnl
dnl This source code is licensed under the MIT license found in the
dnl LICENSE file in the root directory of this source tree.

AC_DEFUN([AC_ASSERT_OCAML_MIN_VERSION],
[dnl
  AC_REQUIRE([AC_PROG_OCAML])

  AC_MSG_CHECKING([if OCaml version is >= $1])

  AC_COMPARE_VERSION_STRINGS([$1], [$OCAMLVERSION],
    [AC_MSG_RESULT([yes])],
    [AC_MSG_ERROR([m4_join([],[found version $OCAMLVERSION.
      m4_newline([  Please upgrade to OCaml >= $1. If you are using opam, you can run])
      m4_newline([dnl
    opam switch $1
    eval \$(opam env)])
])])])
])
