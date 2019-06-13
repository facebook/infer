dnl Copyright (c) Facebook, Inc. and its affiliates.
dnl
dnl This source code is licensed under the MIT license found in the
dnl LICENSE file in the root directory of this source tree.

dnl AC_CHECK_PYTHON_MODULE([python],[module])
dnl
dnl checks if the given module is available from the given Python interpreter
AC_DEFUN([AC_CHECK_PYTHON_MODULE],
[dnl
  AC_MSG_CHECKING([for Python module $2])
  if printf "import %s" $2 | $1 - 1> /dev/null 2> /dev/null; then
    AC_MSG_RESULT([ok])
    AS_TR_SH([PYTHON_$2])=yes
  else
    AC_MSG_RESULT([unavailable])
    AS_TR_SH([PYTHON_$2])=no
  fi

  AC_SUBST(AS_TR_SH([PYTHON_$2]))
])
