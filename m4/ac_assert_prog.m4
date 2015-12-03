dnl Copyright (c) 2015 - present Facebook, Inc.
dnl All rights reserved.
dnl
dnl This source code is licensed under the BSD style license found in the
dnl LICENSE file in the root directory of this source tree. An additional grant
dnl of patent rights can be found in the PATENTS file in the same directory.

dnl AC_ASSERT_PROG([program_name], [test_variable])
dnl
dnl fails if $test_variable is "no", which is taken to mean that
dnl $program_name is not installed
AC_DEFUN([AC_ASSERT_PROG],
[dnl
  AS_IF([test "$2" = "no"],
  [dnl
    AC_MSG_ERROR([$1 not found.])
  ])
])
