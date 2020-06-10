/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdint.h>

int foo_constant() {
  int i, j;
  i = 17;
  j = 31;
  return i + j + 3 + 7;
}

int cond_constant(int i) {
  int x;

  if (i < 0) {
    x = foo_constant();
  } else {
    x = 1;
  }
  return x;
}

int loop0_constant() {

  for (int i = 0; i < 100; i++) {
    foo_constant();
  }
  return 0;
}

int loop1_constant() {

  int k = 100;
  for (int i = 0; i < k; i++) {
    foo_constant();
  }
  return 0;
}

// Expected: O(k)
int loop2_linear(int k) {

  for (int i = 0; i < k; i++) {
    alias2();
  }
  return 0;
}

int loop3_constant(int k) {

  for (int i = k; i < k + 15; i++) {
    alias2();
  }
  return 0;
}

// Expected: O(20-m)
int while_upto20(int m) {
  while (m < 20) {
    int l = 0;
    m++;
  }
  return m;
}

void call_while_upto20_minus100_constant() { while_upto20(-100); }

void call_while_upto20_10_constant() { while_upto20(10); }

void call_while_upto20_unsigned(unsigned x) { while_upto20(x); }

// Cost: 1
void unit_cost_function() {}

int always(int i) { return i % 2 == (i + 2) % 2; }

void infinite_FN() {
  int z;
  for (int i = 0; always(i); i++) {
    z += i;
  }
}

void infinite() {
  int z;
  for (int i = 0; i % 2 == (i + 2) % 2; i++) {
    z += i;
  }
}

void call_infinite() { infinite(); }

void loop_character_symbols_linear(char c) {
  for (; c < 'z';) {
    if (rand()) {
      c = 'a';
    }
  }
}

unsigned int div_const(unsigned int n) { return n / 2; }

void iter_div_const_constant() {
  unsigned int n = div_const(20);
  for (int i = 0; i < n; i++) {
  }
}
