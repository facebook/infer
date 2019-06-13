/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

template <class T>
struct Container {
  T field;
};

struct X {
  int field;
};

int div0_template_field(Container<int>& v) {
  v.field = 0;
  return 1 / v.field;
}

int div0_struct_field(X& v) {
  v.field = 0;
  return 1 / v.field;
}
