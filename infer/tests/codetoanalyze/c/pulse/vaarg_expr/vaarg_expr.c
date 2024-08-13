/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
