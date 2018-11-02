/*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdint.h>

void modulo_signed_Bad(int i) {
  char arr[5];
  arr[i % 5] = 123;
}

void modulo_signed_Good(int i) {
  char arr[5];
  if (i >= 0) {
    arr[i % 5] = 123;
  }
}

void modulo_signed_neg_Bad(int i) {
  char arr[5];
  arr[i % -5] = 123;
}

void modulo_signed_neg_Good(int i) {
  char arr[5];
  if (i >= 0) {
    arr[i % -5] = 123;
  }
}

void modulo_signed_Good2(int i) {
  char arr[5];
  int j = i % 5;
  if (j >= 0) {
    arr[j] = 123;
  }
}

void modulo_unsigned_Good(unsigned int i) {
  char arr[5];
  arr[i % 5] = 123;
}

void modulo_unsigned_short_Good(uint16_t i) {
  char arr[5];
  arr[i % 5] = 123;
}

void modulo_signed_var_Bad_FN(unsigned int len, int i) {
  char arr[len];
  arr[i % len] = 123;
}

void modulo_unsigned_var_Good(unsigned int len, unsigned int i) {
  char arr[len];
  arr[i % len] = 123;
}

unsigned int modulo_unsigned(unsigned int a, unsigned int b) { return a % b; }

void modulo_call_Good(unsigned int len, unsigned int i) {
  char arr[len];
  arr[modulo_unsigned(i, len)] = 123;
}

int modulo_signed(int a, int b) { return a % b; }

void modulo_call_Bad_FN(unsigned int len, int i) {
  char arr[len];
  arr[modulo_signed(i, len)] = 123;
}

int division_of_zero_Good(int x) {
  int i = 4 * x;
  i /= 2;
  i /= 2;
  return i;
}

/* While the most precise return value is
   - "2*i+1"    if 0 <= i < 10,
   - "0"        o.w.
   Inferbo returns [1+min(-1,s0),10+max(-10,s1)] where i is [s0,s1]. */
int plus_linear_min(int i) { /* i |-> [s0,s1] */
  int linear = i + 1; /* linear |-> [s0+1,s1+1] */
  if (i >= 0 && i < 10) { /* i |-> [max(0,s0),min(9,s1)] */
    return linear + i; /* return |-> [s0+1,s1+10] */
  }
  return 0;
}

void plus_linear_min_Good() {
  int a[20];
  a[plus_linear_min(9)] = 1;
}

void plus_linear_min_Bad() {
  int a[19];
  a[plus_linear_min(9)] = 1;
}

void plus_linear_min2_Good_FP() {
  int a[10];
  a[plus_linear_min(4)] = 1;
}

void plus_linear_min3_Good_FP() {
  int a[20];
  a[plus_linear_min(15)] = 1;
}

void integer_overflow_by_addition_Bad() {
  char arr[10];
  int32_t x = 2000000000;
  int32_t y = 2000000000;
  int32_t z = x + y; // z is a negative number.
  if (z < 10) {
    arr[z] = 0;
  }
}

void integer_overflow_by_addition_l2_Bad(int x) {
  int32_t y;
  if (x) {
    y = 0;
  } else {
    y = 2000000000;
  }
  y = y + y;
}

void integer_overflow_by_subtraction_Bad() {
  char arr[10];
  int32_t x = -2000000000;
  int32_t y = 2000000000;
  int32_t z = x - y; // z is a big positive number.
  if (z >= 0) {
    arr[z] = 0;
  }
}

void integer_overflow_by_multiplication_Bad() {
  char arr[10];
  int32_t x = 300000;
  int32_t y = 300000;
  int32_t z = x * y; // z is a negative number.
  if (z < 10) {
    arr[z] = 0;
  }
}

void use_int64_max_Good() {
  char arr[10];
  int64_t x = INT64_MAX;
  int64_t y = INT64_MAX - 5;
  arr[x - y] = 0;
}

void use_int64_max_Bad() {
  char arr[10];
  int64_t x = INT64_MAX;
  int64_t y = INT64_MAX - 15;
  arr[x - y] = 0;
}

void use_uint64_max_Good() {
  char arr[10];
  uint64_t x = UINT64_MAX;
  uint64_t y = UINT64_MAX - 5;
  arr[x - y] = 0;
}

void use_uint64_max_Bad() {
  char arr[10];
  uint64_t x = UINT64_MAX;
  uint64_t y = UINT64_MAX - 15;
  arr[x - y] = 0;
}

uint64_t unknown_uint();

void muliply_one_Good() {
  uint64_t x = unknown_uint();
  uint64_t y = x * 1;
}

void muliply_two_Bad() {
  uint64_t x = unknown_uint();
  uint64_t y = x * 2;
}

void minus_one_Good() {
  uint64_t x = unknown_uint();
  if (x > 0) {
    uint64_t y = x - 1;
  }
}

void minus_one_Bad() {
  uint64_t x = unknown_uint();
  if (x >= 0) {
    uint64_t y = x - 1;
  }
}

int64_t unknown_int();

void plus_one_Good() {
  int64_t x = unknown_int();
  if (x < INT64_MAX) {
    int64_t y = x + 1;
  }
}

void plus_one_Bad() {
  int64_t x = unknown_int();
  if (x <= INT64_MAX) {
    int64_t y = x + 1;
  }
}

void minus_minimum_Good() {
  int64_t x = -1;
  int64_t y = x - INT64_MIN;
}

void minus_minimum_Bad() {
  int64_t x = 0;
  int64_t y = x - INT64_MIN;
}

void mult_minimum_Good() {
  int64_t x = 1;
  int64_t y = x * INT64_MIN;
}

void mult_minimum_Bad() {
  int64_t x = -1;
  int64_t y = x * INT64_MIN;
}

void unsigned_prune_zero1_Good(unsigned int x) {
  if (x != 0) {
    unsigned int y = x - 1;
  }
}

void call_unsigned_prune_zero1_Good() { unsigned_prune_zero1_Good(0); }

void unsigned_prune_zero2_Good(unsigned int y) {
  unsigned int x = y;
  for (; x; --x) {
  }
}

void call_unsigned_prune_zero2_Good_FP() { unsigned_prune_zero2_Good(0); }

void unsigned_prune_ge1_Good(unsigned int x, unsigned int y) {
  if (x >= y) {
    unsigned int z = x - y;
  }
}

void call_unsigned_prune_ge1_Good_FP() { unsigned_prune_ge1_Good(0, 1); }

void unsigned_prune_ge2_Good(unsigned int x, unsigned int y) {
  if (y > 0) {
    if (x >= y) {
      unsigned int z = x - 1;
    }
  }
}

void call_unsigned_prune_ge2_Good() { unsigned_prune_ge2_Good(0, 1); }

void unsigned_prune_ge3_Good(unsigned int x, unsigned int y) {
  if (y > 0) {
    if (x >= y + 1) {
      unsigned int z = x - 1;
    }
  }
}

void call_unsigned_prune_ge3_Good() { unsigned_prune_ge3_Good(0, 1); }

void unsigned_prune_gt(unsigned int x, unsigned int y) {
  if (x > 0) {
    unsigned int z = x - y;
  }
}

void call_unsigned_prune_gt_Good() { unsigned_prune_gt(0, 3); }

void minmax_div_const_Good(int n) {
  int a[9];
  if (0 < n && n < 65) {
    int x = a[n / 8];
  }
}

void minmax_div_const_Bad(int n) {
  int a[7];
  if (0 < n && n < 65) {
    int x = a[n / 8];
  }
}

uint32_t unknown_nat() {
  uint32_t x = unknown_function();
  if (x >= 0) {
    return x;
  } else {
    return 0;
  }
}

void two_safety_conditions2_Bad(uint32_t s) {
  uint32_t x = unknown_nat();
  uint32_t y, z;

  if (unknown_function()) {
    y = 0;
  } else {
    y = 80;
  }
  z = x + y; // integer overflow L5: [0, +oo] + [0, 80]

  if (s >= 10 && s <= 20) {
    z = x + s; // [0, +oo] + [max(10, s.lb), min(20, s.ub)]
  }
}

void call_two_safety_conditions2_Bad() {
  two_safety_conditions2_Bad(15); // integer overflow L5: [0, +oo] + 15
}
