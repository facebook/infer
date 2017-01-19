/*
 * Copyright (c) 2016 - present
 *
 * Programming Research Laboratory (ROPAS)
 * Seoul National University, Korea
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stdlib.h>

void do_while_sub(char* a, int len) {
  int i = 0;
  do {
    a[i] = i;
    i++;
  } while (i < len);
}

void do_while() {
  char* a = malloc(10);
  do_while_sub(a, 10); /* SAFE */
  do_while_sub(a, 11); /* BUG */
}
