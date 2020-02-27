/*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

void call_unsigned_prune_zero2_Good() { unsigned_prune_zero2_Good(0); }

void unsigned_prune_ge1_Good(unsigned int x, unsigned int y) {
  if (x >= y) {
    unsigned int z = x - y;
  }
}

void call_unsigned_prune_ge1_Good() { unsigned_prune_ge1_Good(0, 1); }

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

void div_const_Good() {
  int a[3];
  int x = 5 / 2;
  a[x] = 0;
}

void div_const_Bad() {
  int a[2];
  int x = 5 / 2;
  a[x] = 0;
}

void div_const2_FP(int n) {
  int a[1];
  int x = (n * 2 + 1) / 2;
  a[x] = 0;
}

void minmax_div_const2_Good() {
  div_const2(-1);
  div_const2(0);
}

void minmax_div_const2_Bad_FN() {
  div_const2(1);
  div_const2(-2);
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

void band_positive_constant_Good() {
  char a[3];
  int x = 6 & 2; // y is 2
  a[x] = 0;
}

void band_positive_constant_Bad() {
  char a[2];
  int x = 6 & 2; // y is 2
  a[x] = 0;
}

void band_negative_constant_Good() {
  char a[1];
  int x = (-3) & (-2); // x is -4
  a[x + 4] = 0;
}

void band_negative_constant_Bad() {
  char a[1];
  int x = (-3) & (-2); // x is -4
  a[x + 5] = 0;
}

void band_constant_Good() {
  char a[2];
  int x = (-3) & 1; // x is 1
  a[x] = 0;
}

void band_constant_Bad() {
  char a[1];
  int x = (-3) & 1; // x is 1
  a[x] = 0;
}

void band_positive_Good() {
  char a[9];
  int x = unknown_nat();
  int y = unknown_nat();
  if (x <= 10 && y <= 8) {
    int z = x & y; // z is [0, 8]
    a[z] = 0;
  }
}

void band_positive_Bad() {
  char a[5];
  int x = unknown_nat();
  int y = unknown_nat();
  if (x <= 10 && y <= 8) {
    int z = x & y; // z is [0, 8]
    a[z] = 0;
  }
}

void band_negative_Good() {
  char a[3];
  int x = unknown_function();
  int y = unknown_function();
  if (x <= -3 && y <= -2) {
    int z = x & y; // z is [-oo, -3]
    z = z + 5; // z is [-oo, 2]
    if (z >= 0) {
      a[z] = 0;
    }
  }
}

void band_negative_Bad() {
  char a[2];
  int x = unknown_function();
  int y = unknown_function();
  if (x <= -3 && y <= -2) {
    int z = x & y; // z is [-oo, -3]
    z = z + 5; // z is [-oo, 2]
    if (z >= 0) {
      a[z] = 0;
    }
  }
}

#define FOUR_GIGABYTES 0xFFFFFFFF
#define ALMOST_FOUR_GIGABYTES (85 * FOUR_GIGABYTES / 100)

void simple_overflow_Bad() { auto x = ALMOST_FOUR_GIGABYTES; }

unsigned int unused_integer_underflow_Good_FP() {
  unsigned int n = 0;
  if (n-- == 0) {
    return 0;
  } else {
    return n;
  }
}

unsigned int unused_integer_underflow_Bad() {
  unsigned int n = 0;
  if (n-- == 0) {
    return n;
  } else {
    return n;
  }
}

unsigned int unused_integer_underflow2_Good_FP() {
  unsigned int n = 0;
  return n--;
}

unsigned int unused_integer_underflow2_Bad() {
  unsigned int n = 0;
  return --n;
}

void recover_integer_underflow_Good_FP() {
  for (unsigned int i = 0; i < 10; i++) {
    if (unknown_function()) {
      i--; // right after this, i++ will be called.
    }
  }
}

void recover_integer_underflow_Bad() {
  for (unsigned int i = 0; i < 10; i++) {
    if (unknown_function()) {
      i -= 2;
    }
  }
}

unsigned long scan_hex_Good(char* cp) {
  unsigned long num_digits = 0, digit, val = 0;
  while (1) {
    digit = *cp;
    if ((digit - '0') <= 9)
      digit -= '0';
    else if ((digit - 'a') < 6)
      digit -= 'a' - 10;
    else if ((digit - 'A') < 6)
      digit -= 'A' - 10;
    else
      break;
    val = (val << 4) | digit;
    ++cp;
  }
  return val;
}

void call_scan_hex_Good_FP() {
  char* cp = "\0";
  scan_hex_Good(cp);
}

void call_scan_hex2_Good_FP() {
  char* cp = "0aA";
  scan_hex_Good(cp);
}

int check_addition_overflow_Good(unsigned int x, unsigned int y) {
  if (((unsigned int)-1) - x < y) {
    return 1;
  } else {
    return 0;
  }
}

void shift_right_zero_Good(int x) {
  int arr[1];
  arr[0 >> x] = 1;
}

void shift_right_zero_Bad(int x) {
  int arr[1];
  arr[1 + (0 >> x)] = 1;
}

#define INTENDED_INTEGER_UNDERFLOW (0ULL - 2)

void use_intended_integer_underflow_Good() {
  unsigned long long x = INTENDED_INTEGER_UNDERFLOW;
}

void ptr_band1_Bad(int x, int* q) {
  int a[5];
  int* p;
  if (x) {
    p = q;
  } else {
    p = 0;
  }
  if ((int)p & x) {
    a[10] = 0;
  }
}

void ptr_band2_Bad(int x, int* q) {
  int a[5];
  int* p;
  if (x) {
    p = q;
  } else {
    p = 0;
  }
  if ((int)p & x) {
  }
  a[10] = 0;
}

void do_not_prune_float_Good_FP() {
  int a[5];
  float f = 0.5;
  if (f > 1.0 && f < 1.0) {
    a[10] = 0;
  }
}

void do_not_prune_float_Bad() {
  int a[5];
  float f = 0.5;
  if (f > 0.0 && f < 1.0) {
    a[10] = 0;
  }
}
