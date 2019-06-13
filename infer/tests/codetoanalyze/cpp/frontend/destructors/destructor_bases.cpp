/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct T {
  T(){};
  ~T(){};
};

struct A : virtual T {
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

struct D : A, C {
  B b;
  D(){};
  ~D() { A a; };
};

struct E : B, C, D {
  E(){};
  ~E(){};
};

struct F : B, virtual C, D {
  F(){};
  ~F(){};
};
