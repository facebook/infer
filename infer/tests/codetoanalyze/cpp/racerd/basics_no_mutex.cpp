/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace basics {

class BasicsNoMutex {
 public:
  BasicsNoMutex() {}

  void set_ok(int new_value) {
    field_1 = new_value;
    field_2 = new_value;
  }

  int get_field1_ok() { return field_1; }

 private:
  int field_1;
  int field_2;
};
} // namespace basics
