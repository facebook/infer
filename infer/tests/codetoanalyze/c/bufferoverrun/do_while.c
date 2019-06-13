/*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

void do_while_sub(char* a, int len) {
  int i = 0;
  do {
    a[i] = i;
    i++;
  } while (i < len);
}

void do_while_Good() {
  char* a = malloc(10);
  do_while_sub(a, 10); /* SAFE */
}

void do_while_Bad() {
  char* a = malloc(10);
  do_while_sub(a, 11); /* BUG */
}

void do_while_2_Good() {
  int count = 10;
  int a[count];
  int i = 0;
  do {
    a[i] = 0;
  } while (++i < count);
}

void do_while_2_Bad() {
  int count = 10;
  int a[count];
  int i = 0;
  do {
    a[i] = 0;
  } while (i++ < count);
}
