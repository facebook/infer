dnl Copyright (c) 2015 - present Facebook, Inc.
dnl All rights reserved.
dnl
dnl This source code is licensed under the BSD style license found in the
dnl LICENSE file in the root directory of this source tree. An additional grant
dnl of patent rights can be found in the PATENTS file in the same directory.

AC_DEFUN([AC_OCAML_CHECK_NATIVE_BYTES],
[dnl
  OCAML_HAS_NATIVE_BYTES=no
  AC_MSG_CHECKING([if OCaml supports the native 'bytes' type])
  cat > conftest.ml <<EOF
  let b:bytes = Bytes.create 4
EOF
  if $OCAMLC -c conftest.ml >&5 2>&5 ; then
    AC_MSG_RESULT([yes])
    OCAML_HAS_NATIVE_BYTES=yes
  else
    AC_MSG_RESULT([no])
  fi
  AC_SUBST([OCAML_HAS_NATIVE_BYTES])
])
