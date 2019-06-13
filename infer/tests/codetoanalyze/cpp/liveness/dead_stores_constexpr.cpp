/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
int foo(int a);

// before, we were raising DEAD_STORE for x
void capture_constexpr_good() {
  constexpr int x = 10;
  []() {
    foo(x);
    return;
  }();
}

void call_it(void (*f)(void)) { f(); }

void capture_constexpr2_good() {
  constexpr int x = 1;
  auto lambda = []() {
    foo(x);
    return;
  };
  call_it(lambda);
}

void capture_const_bad(const int y) {
  const int x = y;
  []() {
    foo(0);
    return;
  }();
}

// we always assume const exprs are captured in lambdas
void FN_capture_constexpr_good() {
  constexpr int x = 10;
  []() {
    foo(0);
    return;
  }();
}

void FN_init_capture_reassign_bad() {
  constexpr int i = 1; // this is a dead store
  return [i = 1]() { return i; }();
}

// expected DEAD_STORE
void FN_capture_constexpr_bad() {
  constexpr int x = 1;
  foo(2);
}

// expected since dead stores to a "sentinel" value are ignored
void capture_constexpr_sentinel_good() {
  constexpr int x = 0;
  []() { return; }();
}
