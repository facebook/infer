/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stdarg.h>

int vaarg_foo(int x, ...) {
  va_list valist;
  va_start(valist, x);
  int i = va_arg(valist, int);
  int val;
  if (i == 9) {
    val = 9 / 0;
  } else {
    val = 4 / 0;
  }
  va_end(valist);
  return val;
}
