/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace skip_model {
struct SkipAll {
  int* foo() { return nullptr; }
  static int* goo() { return nullptr; }
};

template <typename T>
struct SkipSome {
  int* skip_me(int**) { return nullptr; }
  static int no_skip() { return 42; }
};

void test_config_no_skip_ok() {
  if (SkipSome<SkipSome<int>>::no_skip() != 42) {
    int* p = nullptr;
    *p = 42;
  }
}

void test_config_skip_me_ok() {
  int* may_be_mutated = nullptr;
  int* p = nullptr;
  SkipSome<SkipSome<int>> x;
  p = x.skip_me(&may_be_mutated);
  *p = 42;
  *may_be_mutated = 42;
}
} // namespace skip_model

namespace skip_another_namespace {
void test_config_foo_ok() {
  int* p = nullptr;
  skip_model::SkipAll x;
  p = x.foo();
  *p = 42;
}
void test_config_goo_ok() {
  int* p = skip_model::SkipAll::goo();
  *p = 42;
}

} // namespace skip_another_namespace
