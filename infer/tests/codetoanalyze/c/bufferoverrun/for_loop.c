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

char* safealloc(int n) {
  char* x;
  if (n > 0)
    x = malloc(n);
  else
    x = malloc(10);

  if (!x)
    return x;
  else
    exit(1);
}

void for_loop() {
  char* a;
  int i;

  a = safealloc(10);
  for (i = 0; i < 10; i++) {
    a[i] = 'a'; /* SAFE */
  }
  a = safealloc(5);
  for (i = 0; i < 10; i++) {
    a[i] = 'a'; /* BUG */
  }
}
