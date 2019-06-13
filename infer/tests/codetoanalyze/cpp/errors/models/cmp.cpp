/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <functional>

struct A {
  int x, y;
};

bool operator==(const A& lhs, const A& rhs) {
  return lhs.x == rhs.x && lhs.y == rhs.y;
}
bool operator!=(const A& lhs, const A& rhs) { return !(lhs == rhs); }
bool operator<(const A& lhs, const A& rhs) {
  if (lhs.x < rhs.x)
    return true;
  else if (lhs.x > rhs.x)
    return false;
  else
    return lhs.y < rhs.y;
}
bool operator>(const A& lhs, const A& rhs) { return rhs < lhs; }
bool operator>=(const A& lhs, const A& rhs) { return !(lhs < rhs); }
bool operator<=(const A& lhs, const A& rhs) { return !(rhs < lhs); }

void operator_eq_ok(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y};
  if (!(a == b))
    *p = 42;
}

void operator_eq_bad(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y};
  if (a == b)
    *p = 42;
}

void operator_neq_ok(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y};
  if (a != b)
    *p = 42;
}

void operator_neq_bad(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y};
  if (!(a != b))
    *p = 42;
}

void std_equal_to_ok(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y};
  if (!(std::equal_to<A>()(a, b)))
    *p = 42;
}

void std_not_equal_to_ok(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y};
  if (std::not_equal_to<A>()(a, b))
    *p = 42;
}

void std_equal_to_bad(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y};
  if (std::equal_to<A>()(a, b))
    *p = 42;
}

void std_not_equal_to_bad(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y};
  if (!(std::not_equal_to<A>()(a, b)))
    *p = 42;
}

void operator_lt_ok(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y + 1};
  if (!(a < b))
    *p = 42;
}

void operator_lt_bad(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y + 1};
  if (a < b)
    *p = 42;
}

void operator_le_ok(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y + 1};
  if (!(a <= b))
    *p = 42;
}

void operator_le_bad(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y + 1};
  if (a <= b)
    *p = 42;
}

void operator_gt_ok(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y - 1};
  if (!(a > b))
    *p = 42;
}

void operator_gt_bad(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y - 1};
  if (a > b)
    *p = 42;
}

void operator_ge_ok(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y - 1};
  if (!(a >= b))
    *p = 42;
}

void operator_ge_bad(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y - 1};
  if (a >= b)
    *p = 42;
}

void std_less_ok(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y + 1};
  if (!(std::less<A>()(a, b)))
    *p = 42;
}

void std_less_bad(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y + 1};
  if (std::less<A>()(a, b))
    *p = 42;
}

void std_less_equal_ok(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y + 1};
  if (!(std::less_equal<A>()(a, b)))
    *p = 42;
}

void std_less_equal_bad(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y + 1};
  if (std::less_equal<A>()(a, b))
    *p = 42;
}

void std_greater_ok(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y - 1};
  if (!(std::greater<A>()(a, b)))
    *p = 42;
}

void std_greater_bad(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y - 1};
  if (std::greater<A>()(a, b))
    *p = 42;
}

void std_greater_equal_ok(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y - 1};
  if (!(std::greater_equal<A>()(a, b)))
    *p = 42;
}

void std_greater_equal_bad(const A& a) {
  int* p = nullptr;
  A b{a.x, a.y - 1};
  if (std::greater_equal<A>()(a, b))
    *p = 42;
}
