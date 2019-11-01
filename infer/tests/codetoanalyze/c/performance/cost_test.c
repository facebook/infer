/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdint.h>

// Cost: 5
int foo_OK() {
  int i, j;
  i = 17;
  j = 31;

  return i + j + 3 + 7;
}

// Cost: 17
int bar_OK() {

  int j = 0;

  j++;
  j++;
  j++;
  j = foo_OK();
  j++;

  return j;
}

// Cost: 25
int cond_OK(int i) {
  int x;

  if (i < 0) {
    x = bar_OK();
  } else {
    x = 1;
  }
  return x;
}

// Cost: 5
void alias_OK() {

  int i, j;

  j = i;
  i = ++i;
}

// Cost: 6
void alias2_OK() {

  int i, j, z;

  j = 1;
  z = 2;

  j = i;
  i = z;
}

// Cost: 1004
int loop0_bad() {

  for (int i = 0; i < 100; i++) {
    alias2_OK();
  }
  return 0;
}

// Cost: 1006
int loop1_bad() {

  int k = 100;
  for (int i = 0; i < k; i++) {
    alias2_OK();
  }
  return 0;
}

// Expected: Theta(k)
int loop2_bad(int k) {

  for (int i = 0; i < k; i++) {
    alias2_OK();
  }
  return 0;
}

// Expected: ~15
int loop3_bad(int k) {

  for (int i = k; i < k + 15; i++) {
    alias2_OK();
  }
  return 0;
}

// Cost: 218
// Shows that calling many times non expensive function can
// result in an expensive computation
int main_bad() {

  int k1, k2, k3, k4;

  cond_OK(2);
  k1 = bar_OK() + foo_OK() + cond_OK(15) * 2;
  k2 = bar_OK() + foo_OK() + cond_OK(17) * 3;
  k3 = bar_OK() + foo_OK() + cond_OK(11) * 3;
  k4 = bar_OK() + foo_OK() + cond_OK(19) * 3;
  return 0;
}

// Expected: Theta(20-m)
int while_upto20_bad(int m) {
  while (m < 20) {
    int l = 0;
    m++;
  }
  return m;
}

void call_while_upto20_minus100_bad() { while_upto20_bad(-100); }

void call_while_upto20_10_good() { while_upto20_bad(10); }

void call_while_upto20_unsigned_good(unsigned x) { while_upto20_bad(x); }

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

// Cost should not include the symbol of c.
void ignore_character_symbols_constant_FP(char c) {
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
