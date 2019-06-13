/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
class T {
 private:
  int* unnanotated_field;
  int* _Nullable nullable_field;
  int* _Nonnull nonnull_field;

 public:
  void assign_nullable_field_to_null_okay() { nullable_field = nullptr; }

 public:
  void assign_unnanotated_field_to_null_bad() { unnanotated_field = nullptr; }

 public:
  void assign_nonnull_field_to_null_bad() { nonnull_field = nullptr; }

 public:
  void test_nullable_field_for_null_okay() {
    if (nullable_field == nullptr) {
    }
  }

 public:
  void test_unnanotated_field_for_null_bad() {
    if (unnanotated_field == nullptr) {
    }
  }

 public:
  void test_nonnull_field_for_null_bad() {
    if (nonnull_field == nullptr) {
    }
  }

 public:
  void dereference_unnanotated_field_okay() { *unnanotated_field = 42; }

 public:
  void dereference_nonnull_field_okay() { *nonnull_field = 42; }

 public:
  void dereference_nullable_field_bad() { *nullable_field = 42; }

 public:
  void dereference_unnanotated_field_after_test_for_null_bad() {
    if (unnanotated_field == nullptr) {
      *unnanotated_field = 42;
    }
  }

 public:
  void FP_dereference_nonnull_field_after_test_for_null_okay() {
    if (nonnull_field == nullptr) {
      *nonnull_field = 42;
    }
  }
};
