/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

void error_under_true_conditionals_bad(int* x) {
  if (1) {
    free(x);
  }
  if (2 == 2) {
    *x = 42;
  }
}

void simple_infeasible_error_path_ok(int* x) {
  free(x);
  if (0 == 1) {
    *x = 42;
  }

  int y = 0;
  if (y == 1) {
    *x = 42;
  }
  if (y) {
    *x = 42;
  }
  if (y != 0) {
    *x = 42;
  }
  if (!(y == 0)) {
    *x = 42;
  }
  if (!(!(y != 0))) {
    *x = 42;
  }
  if (!(!(!(0 == y)))) {
    *x = 42;
  }
}

void free_if(int* x, int b) {
  if (b) {
    free(x);
  }
}

void no_free_if_ok(int* x) {
  free_if(x, 0);
  *x = 42;
}

void free_if_deref_bad(int* x) {
  free_if(x, 1);
  *x = 42;
}

void infeasible_tricky_ok(int* x) {
  free_if(x, 1);
  int y = 42;
  if (2 * y != y << 1) {
    free(x);
    *x = 42;
  }
}

int minus(int x, int y) { return x - y; }

void function_call_infeasible_error_path_ok(int* x) {
  free(x);
  if (minus(0, 0) < 0) {
    *x = 42;
  }
}

// somewhat like folly::Range<char const*>
struct StringRange {
  char const *b_, *e_;
  StringRange() : b_(), e_(){};
  char const* data() const { return b_; }
  size_t size() const { return size_t(e_ - b_); }
};

void function_empty_range_ok() {
  StringRange x{};
  auto b = x.data(), past = x.data() + x.size();
  for (;; ++b) {
    if (b >= past) {
      return;
    }
    if (*b != ' ') {
      break;
    }
  }
}

void find_first_non_space(StringRange& x) {
  auto b = x.data(), past = x.data() + x.size();
  for (;; ++b) {
    if (b >= past) {
      return;
    }
    if (*b != ' ') {
      break;
    }
  }
}

void function_empty_range_interproc_ok() {
  StringRange x{};
  find_first_non_space(x);
}

// arithmetic on integers does not wrap around but ignores too-large
// values. However, somehow the FP is gone for other reasons.
void int_over_cap_ok() {
  unsigned long one = 1;
  // 2^(63+63+3) + 2*2^(63+3) + 1*8 = 2^129 + 2^67 + 8 = 8 mod 2^64
  // this is convoluted to escape various simplifications from Z that would
  // avoid the false positive
  unsigned long x = ((one << 62) * 2 + 1) * ((one << 62) * 2 + 1) * 8;
  unsigned long y = ((one << 62) * 2 + 1) * ((one << 62) * 2 + 1) * 8;
  // - x == y+1 is true in "Formulas" because x = y = Q.undef, but not true in
  //   inferbo intervals because they keep arbitrary precision integers
  // - x != 8 is not true in Formulas but true in inferbo
  // - In C both of these would be false, so overall we get a false positive
  if (x == y + 1 || x != 8) {
    int* p = nullptr;
    *p = 42;
  }
}

void int_under_cap_ok() {
  unsigned long one = 1;
  // 2^63
  unsigned long x = (one << 62) * 2;
  if (x != 9223372036854775808UL) {
    int* p = nullptr;
    *p = 42;
  }
}

// used to confuse inferbo
int mult(int x, int y) { return x * y; }

void ints_are_not_rationals_ok() {
  int x = 5 / 2;
  if (x != mult(2, 1)) {
    int* p = nullptr;
    *p = 42;
  }
}

void shift_equal_mult_by_power_of_two_ok(int x) {
  if (x << 1 != mult(2, x)) {
    int* p = nullptr;
    *p = 42;
  }
}

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshift-count-overflow"
void shift_by_too_much_ok(int x) {
  if (x << 64 != 0 || x >> 4000 != 0) {
    int* p = nullptr;
    *p = 42;
  }
}
#pragma clang diagnostic pop

void interproc_mult_ok(int v, int w) {
  if (mult(32, 52) != 1664 || mult(10, v) != 10 * v || mult(v, w) != v * w) {
    int* p = nullptr;
    *p = 42;
  }
}
