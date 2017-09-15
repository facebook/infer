/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
class T {
 private:
  int* _Nullable nullable_field;
  int* unnanotated_field;

 public:
  void assign_nullable_field_to_null_okay() { nullable_field = nullptr; }

 public:
  void assign_unnanotated_field_to_null_bad() { unnanotated_field = nullptr; }

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
  void FN_dereference_nullable_field_bad() { *nullable_field = 42; }
};
