dnl Copyright (c) 2015 - present Facebook, Inc.
dnl All rights reserved.
dnl
dnl This source code is licensed under the BSD style license found in the
dnl LICENSE file in the root directory of this source tree. An additional grant
dnl of patent rights can be found in the PATENTS file in the same directory.

AC_DEFUN([AC_OCAMLC_CHECK_SAFE_STRING],
[dnl
  OCAMLC_HAS_SAFE_STRING=no
  AC_MSG_CHECKING([if $OCAMLC supports the '-safe-string' flag])
  printf "" > conftest.ml
  if $OCAMLC -safe-string -c conftest.ml >&5 2>&5 ; then
    AC_MSG_RESULT([yes])
    OCAMLC_HAS_SAFE_STRING=yes
  else
    AC_MSG_RESULT([no])
  fi
  AC_SUBST([OCAMLC_HAS_SAFE_STRING])
])
