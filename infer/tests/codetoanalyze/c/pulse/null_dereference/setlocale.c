/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
