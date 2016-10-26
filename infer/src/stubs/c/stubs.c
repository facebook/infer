/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <sys/ioctl.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

CAMLprim value term_width(value unit) {
  CAMLparam1(unit);

  struct winsize sz;

  int size = 0;
  if (ioctl(0, TIOCGWINSZ, &sz) >= 0) {
    size = sz.ws_col;
  }
  CAMLreturn(Val_int(size));
}
