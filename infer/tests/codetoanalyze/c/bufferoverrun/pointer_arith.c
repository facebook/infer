/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void pointer_arith_bad() {
  char arr[10];
  int x = 0;
  if (&x - 1 == 0)
    arr[10] = 1;
}

void array_pointer_arith_Bad() {
  int arr[10];
  int* p = &arr[5];
  p[5] = 1;
}

void pointer_arith2_Ok(int x) {
  int len = 5;
  char p[5];
  (p + x)[3 - x] = 0;
}

void call_pointer_arith2_Ok() { pointer_arith2_Ok(100); }

void pointer_arith3(char* p, int x) {
  int len = 5;
  (p + x)[10 - x] = 0;
}

void call_pointer_arith3_Bad() {
  char p[5];
  pointer_arith3(p, 100);
}

void pointer_arith4_Bad(int x) {
  int len = 5;
  char p[5];
  (p + x)[10 - x] = 0;
}

/* It is better to raise an alarm here, rather than returning a safety
   condition, since the buffer overrun occurs always without regard to
   the input x.  Using symbols for variables, not only for bounds,
   would help in this case. */
void FN_pointer_arith4_Bad(int* x) {
  int len = 5;
  char p[5];
  (p + *x)[10 - *x] = 0;
}

void call_pointer_arith4_Bad() {
  int x = 100;
  FN_pointer_arith4_Bad(&x);
}

#include <stdio.h>
#include <stdlib.h>

void FP_pointer_arith5_Ok() {
  char buf[1024];
  fgets(buf, 1024, stdin);
  size_t len = strlen(buf);
  if (len < sizeof(buf) - 3) {
    (buf + len)[sizeof(buf) - len - 1] = '\0';
  }
}

void pointer_arith5_Bad() {
  char buf[1024];
  fgets(buf, 1024, stdin);
  size_t len = strlen(buf);
  if (len < sizeof(buf) - 3) {
    (buf + len)[sizeof(buf) - len] = '\0';
  }
}
