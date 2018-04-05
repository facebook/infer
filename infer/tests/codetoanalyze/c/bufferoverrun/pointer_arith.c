/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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

/* It is better to raise an alarm here, rather than returning a safety
   condition, since the buffer overrun occurs always without regard to
   the input x.  Using symbols for variables, not only for bounds,
   would help in this case. */
void FN_pointer_arith4_Bad(int x) {
  int len = 5;
  char p[5];
  (p + x)[10 - x] = 0;
}

void call_pointer_arith4_Bad() { FN_pointer_arith4_Bad(100); }
