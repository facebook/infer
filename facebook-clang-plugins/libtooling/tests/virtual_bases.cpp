/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
struct A {
  A(){};
  ~A(){};
};

struct B : virtual A {
  B(){};
  ~B(){};
};

struct C {
  C(){};
  ~C(){};
};

struct E : B, virtual C {
  E(){};
  ~E(){};
};
