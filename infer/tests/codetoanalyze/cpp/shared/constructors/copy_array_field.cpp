/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
namespace copy_array_field {
struct X {
  int* p;
  int x[10]; // array field
};

int npe() {
  X x1;
  x1.p = 0;
  X x2 = x1; // will call default copy constructor
  return *x2.p;
}

int no_npe() {
  int a = 0;
  X x1;
  x1.p = &a;
  X x2 = x1; // will call default copy constructor
  return *x2.p;
}

struct Small {
  int* p[2];
  int m[2][2];
};

void copy_small(Small& s) { Small s2 = s; }

void assign_small(Small& s1, const Small& s2) { s1 = s2; }

struct Elt {
  int* p;
  Elt() : p(nullptr) {}
  Elt(const Elt& other) : p(other.p) {}
};

struct WithObjects {
  Elt elts[2];
};

void copy_objects(WithObjects& w) { WithObjects w2 = w; }

void construct_objects() { WithObjects w; }

struct ManySmall {
  Small elts[3]; // more than 16 values: not copied element by element
};

void copy_many_small(ManySmall& m) { ManySmall m2 = m; }

void assign_many_small(ManySmall& m1, const ManySmall& m2) { m1 = m2; }

struct ManyArrays {
  int* p[2];
  int a[5];
  int b[5];
  int c[5]; // more than 16 values in all: not copied element by element
};

void copy_many_arrays(ManyArrays& m) { ManyArrays m2 = m; }

void assign_many_arrays(ManyArrays& m1, const ManyArrays& m2) { m1 = m2; }
} // namespace copy_array_field
