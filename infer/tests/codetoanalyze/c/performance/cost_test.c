/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
int foo() {
  int i, j;
  i = 17;
  j = 31;

  return i + j + 3 + 7;
}

int bar() {

  int j = 0;

  j++;
  j++;
  j++;
  j = foo();
  j++;

  return j;
}

int cond(int i) {
  int x;

  if (i < 0) {
    x = bar();
  } else {
    x = 1;
  }
  return x;
}

void alias() {

  int i, j;

  j = i;
  i = ++i;
}

void alias2() {

  int i, j, z;

  j = 1;
  z = 2;

  j = i;
  i = z;
}

int loop0() {

  for (int i = 0; i < 100; i++) {
    alias2();
  }
  return 0;
}

int main() {

  int k;

  cond(2);
  k = bar() + foo() * 2;

  return 0;
}
