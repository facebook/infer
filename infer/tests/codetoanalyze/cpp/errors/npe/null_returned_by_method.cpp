/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class X {
 public:
  int field;
};

class XFactory {
 public:
  X nonNull;
  X* getNull() { return nullptr; }
  X* getNonNull() { return &nonNull; }
};

int testNullDeref(XFactory* factory) {
  if (factory) {
    X* x = factory->getNull();
    return x->field;
  }
}

int testNoNullDeref(XFactory* factory) {
  if (factory) {
    X* x = factory->getNonNull();
    return x->field;
  }
}
