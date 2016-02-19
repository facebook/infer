/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
