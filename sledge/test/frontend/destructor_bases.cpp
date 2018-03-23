/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#pragma clang diagnostic ignored "-Winaccessible-base"

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

int main() { struct F f; }
