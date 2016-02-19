/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
