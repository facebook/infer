/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace structured_binding {
struct S {
  int x;
  int y;
};

void do_structured_binding_ref_ok(S& a) {
  auto& [x, y] = a;
  x = 42;
  y = 52;
}
} // namespace structured_binding
