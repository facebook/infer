dnl Copyright (c) Facebook, Inc. and its affiliates.
dnl
dnl This source code is licensed under the MIT license found in the
dnl LICENSE file in the root directory of this source tree.

AC_DEFUN([AC_COMPARE_VERSION_STRINGS],
[dnl
  unset major_req
  unset minor_req
  unset patch_req
  unset major_inst
  unset minor_inst
  unset patch_inst

  major_req=$(printf "$1" | cut -d . -f 1)
  minor_req=$(printf "$1" | cut -d . -f 2)
  patch_req=$(printf "$1" | cut -d . -f 3)

  major_inst=$(printf "$2" | cut -d . -f 1)
  minor_inst=$(printf "$2" | cut -d . -f 2)
  # discard trailing characters after patch number, eg 1.2.3+4~5 -> 3
  patch_inst=$(printf "$2" | cut -d . -f 3 | grep -o -e '^[[[:digit:]]]*')
  AS_IF([test $major_inst -gt $major_req || \
         (test $major_inst -eq $major_req && \
          (test $minor_inst -gt $minor_req || \
           (test $minor_inst -eq $minor_req && \
            test $patch_inst -ge $patch_req)))],
        $3,
        $4)
])
