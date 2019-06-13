/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace object_deref {

class X {
 public:
  int field;
};

X* getNull() { return nullptr; }

X globalX;
X* getNonNull() { return &globalX; }

void derefNullField() {
  X* x = getNull();
  int c = x->field;
}

void derefNonNullField() {
  X* x = getNonNull();
  int c = x->field;
}
} // namespace object_deref
