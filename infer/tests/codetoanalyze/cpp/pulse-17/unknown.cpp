/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace unknown {

class C {
 public:
  int x;
  int y;
};

static C unknown_get_C();

class D {
  C c;
  int x;

 public:
  D() : c(unknown_get_C()), x(c.x) {}
};

void call_constructor_D_ok() { D d; }

} // namespace unknown
