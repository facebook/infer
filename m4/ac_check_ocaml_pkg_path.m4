dnl Copyright (c) 2015 - present Facebook, Inc.
dnl All rights reserved.
dnl
dnl This source code is licensed under the BSD style license found in the
dnl LICENSE file in the root directory of this source tree. An additional grant
dnl of patent rights can be found in the PATENTS file in the same directory.

dnl this adds the following features to AC_CHECK_OCAML_PKG:
dnl - supports an optional version argument in $3
dnl - sets OCAML_PKG_PATH_$1 to be the path where the package is
dnl   installed
AC_DEFUN([AC_CHECK_OCAML_PKG_PATH],
[dnl
  AC_REQUIRE([AC_PROG_FINDLIB])
  AC_CHECK_OCAML_PKG([$1], [$2])

  unset version_msg
  version_msg=""
  AS_IF([test "x$3" != "x"], [version_msg=" version $3"])
  AC_MSG_CHECKING([for OCaml findlib package $1$version_msg])

  unset path
  unset pkg
  unset version
  pkg=$AS_TR_SH([OCAML_PKG_$1])
  found=no
  if test "x$pkg" != "xno"; then
    path=`$OCAMLFIND query $pkg 2>/dev/null`
    if test "x$3" != "x"; then
      version=`$OCAMLFIND query -format '%v' $pkg 2>/dev/null`
      unset major_req
      unset minor_req
      unset patch_req
      unset major_inst
      unset minor_inst
      unset patch_inst

      major_req=$(printf "$3" | cut -d . -f 1)
      minor_req=$(printf "$3" | cut -d . -f 2)
      patch_req=$(printf "$3" | cut -d . -f 3)

      major_inst=$(printf "$version" | cut -d . -f 1)
      minor_inst=$(printf "$version" | cut -d . -f 2)
      # discard trailing characters after patch number, eg 1.2.3+4~5 -> 3
      patch_inst=$(printf "$version" | cut -d . -f 3 | grep -o -e '^[[[:digit:]]]*')
      if test $major_inst -gt $major_req || \
         (test $major_inst -eq $major_req && \
          (test $minor_inst -gt $minor_req || \
           (test $minor_inst -eq $minor_req && \
            test $path_inst -ge $path_req))); then
        found=yes
      fi
    else
      found=yes
    fi
    if test "$found" = "yes" ; then
      AC_MSG_RESULT([$path])
      AS_TR_SH([OCAML_PKG_$1])=$pkg
      AS_TR_SH([OCAML_PKG_PATH_$1])=$path
    fi
  fi

  if test "$found" = "no" ; then
    AC_MSG_RESULT([not found])
    AS_TR_SH([OCAML_PKG_$1])=no
  fi

  AC_SUBST(AS_TR_SH([OCAML_PKG_$1]))
  AC_SUBST(AS_TR_SH([OCAML_PKG_PATH_$1]))
])
