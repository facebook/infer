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

// that was supposed to be a FP due to tricky arithmetic but inferbo is too
// smart!
void infeasible_tricky_ok(int* x) {
  free_if(x, x == x);
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
  std::size_t size() const { return std::size_t(e_ - b_); }
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
