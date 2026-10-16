/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace using_enum {

enum class Color { kRed, kGreen = 2 };

void using_enum_ok() {
  using enum Color;
  Color c = kGreen;
  if (c != Color::kGreen || static_cast<int>(c) != 2) {
    int* p = nullptr;
    *p = 42;
  }
}

void using_enum_bad() {
  using enum Color;
  Color c = kGreen;
  if (c == Color::kGreen) {
    int* p = nullptr;
    *p = 42;
  }
}

} // namespace using_enum
