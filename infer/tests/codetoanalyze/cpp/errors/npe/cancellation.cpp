/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
namespace cancellation_test {

struct TestIter {
  int y;

  TestIter(int y) : y(y) {}
};

bool operator==(const TestIter& lhs, const TestIter& rhs) {
  return lhs.y == rhs.y;
}

struct Test {
  int x, sz;

  int begin() const { return x; }
  int end() const { return x + sz; }
  int end2() const { return x - sz; }

  TestIter begin_iter() const { return x; }
  TestIter end_iter() const { return x + sz; }
  TestIter end_iter2() const { return x - sz; }
};

bool is_size_zero(const Test& t) { return t.begin() == t.end(); }
bool is_size_zero2(const Test& t) { return t.begin() == t.end2(); }
bool is_size_zero_iter(const Test& t) { return t.begin_iter() == t.end_iter(); }
bool is_size_zero2_iter(const Test& t) {
  return t.begin_iter() == t.end_iter2();
}

void size_zero_no_deref_ok() {
  int* p = nullptr;
  Test t{1, 0};
  if (!is_size_zero(t))
    *p = 42;
}

void size_zero_deref_bad() {
  int* p = nullptr;
  Test t{1, 0};
  if (is_size_zero(t))
    *p = 42;
}

void size_nonzero_no_deref_ok() {
  int* p = nullptr;
  Test t{1, 1};
  if (is_size_zero(t))
    *p = 42;
}

void size_nonzero_deref_bad() {
  int* p = nullptr;
  Test t{1, 1};
  if (!is_size_zero(t))
    *p = 42;
}

void size_nonzero_no_deref2_ok() {
  int* p = nullptr;
  Test t{1, 1};
  if (is_size_zero(t))
    *p = 42;
}

void size_nonzero_deref2_bad() {
  int* p = nullptr;
  Test t{1, 1};
  if (!is_size_zero(t))
    *p = 42;
}

void size_zero_no_deref2_ok() {
  int* p = nullptr;
  Test t{1, 0};
  if (!is_size_zero(t))
    *p = 42;
}

void size_zero_deref2_bad() {
  int* p = nullptr;
  Test t{1, 0};
  if (is_size_zero(t))
    *p = 42;
}

void size_zero_no_deref_iter_ok() {
  int* p = nullptr;
  Test t{1, 0};
  if (!is_size_zero_iter(t))
    *p = 42;
}

void size_zero_deref_iter_bad() {
  int* p = nullptr;
  Test t{1, 0};
  if (is_size_zero_iter(t))
    *p = 42;
}

void size_nonzero_no_deref_iter_ok() {
  int* p = nullptr;
  Test t{1, 1};
  if (is_size_zero_iter(t))
    *p = 42;
}

void size_nonzero_deref_iter_bad() {
  int* p = nullptr;
  Test t{1, 1};
  if (!is_size_zero_iter(t))
    *p = 42;
}

void size_nonzero_no_deref_iter2_ok() {
  int* p = nullptr;
  Test t{1, 1};
  if (is_size_zero_iter(t))
    *p = 42;
}

void size_nonzero_deref_iter2_bad() {
  int* p = nullptr;
  Test t{1, 1};
  if (!is_size_zero_iter(t))
    *p = 42;
}

void size_zero_no_deref_iter2_ok() {
  int* p = nullptr;
  Test t{1, 0};
  if (!is_size_zero_iter(t))
    *p = 42;
}

void size_zero_deref_iter2_bad() {
  int* p = nullptr;
  Test t{1, 0};
  if (is_size_zero_iter(t))
    *p = 42;
}

} // namespace cancellation_test
