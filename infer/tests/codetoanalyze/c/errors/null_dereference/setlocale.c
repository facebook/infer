/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <locale.h>

void nocrash_setlocale() {
  if (setlocale(LC_ALL, NULL) == NULL) {
    // cannot happen
    int* p = NULL;
    *p = 42;
  }
  setlocale(LC_ALL, "");
  setlocale(LC_ALL, "C");
}
