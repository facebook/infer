/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <limits>

namespace if_constexpr {

// the conditions below read library code that is not translated, so only the
// value computed by the compiler tells which branch is taken

template <typename T>
int* assign_if_max_positive(int* p) {
  int* q = nullptr;
  if constexpr (std::numeric_limits<T>::max() > 0) {
    q = p;
  }
  return q;
}

void deref_after_taken_branch_ok() {
  int x = 0;
  int* q = assign_if_max_positive<int>(&x);
  *q = 1;
}

template <typename T>
int* return_if_signed(int* p) {
  if constexpr (std::numeric_limits<T>::is_signed) {
    return p;
  } else {
    return nullptr;
  }
}

void deref_after_then_branch_ok() {
  int x = 0;
  *return_if_signed<int>(&x) = 1;
}

void deref_after_else_branch_bad() {
  int x = 0;
  *return_if_signed<unsigned>(&x) = 1;
}

int* return_if_31_digits(int* p) {
  if constexpr (std::numeric_limits<int>::digits == 31) {
    return p;
  } else {
    return nullptr;
  }
}

void deref_non_template_ok() {
  int x = 0;
  *return_if_31_digits(&x) = 1;
}

template <typename T>
int* return_if_signed_and_wide(int* p) {
  if constexpr (!std::numeric_limits<T>::is_signed)
    return nullptr;
  else if constexpr (std::numeric_limits<T>::digits > 7)
    return p;
  else
    return nullptr;
}

void deref_after_else_if_branch_ok() {
  int x = 0;
  *return_if_signed_and_wide<int>(&x) = 1;
}

void deref_after_last_else_branch_bad() {
  int x = 0;
  *return_if_signed_and_wide<signed char>(&x) = 1;
}

enum { kIntDigits = std::numeric_limits<int>::digits };

void enumerator_initialized_by_library_ok() {
  int* p = nullptr;
  if (kIntDigits != 31) {
    *p = 1;
  }
}

void enumerator_initialized_by_library_bad() {
  int* p = nullptr;
  if (kIntDigits == 31) {
    *p = 1;
  }
}

} // namespace if_constexpr
