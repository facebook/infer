/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// C++20 parenthesized aggregate initialization should be translated like the
// corresponding brace initialization.

struct Point {
  int x;
  int y;
};

struct WithDefault {
  int a;
  int b = 42;
};

struct SuperClass {
  int super_f1;
  int super_f2;
};

struct CurrentClass : public SuperClass {
  int cur_f1;
  int cur_f2;
};

void paren_init_all() { Point p(1, 2); }

void paren_init_some() { Point p(1); }

void paren_init_default_member() { WithDefault w(1); }

void paren_init_with_super() { CurrentClass c(SuperClass(1, 2), 3); }

void paren_init_array() { int a[3](1, 2, 3); }

void paren_init_array_some() {
  int a[3](1);
  Point points[2](Point(1, 2));
}

void paren_init_temporary() { Point p = Point(1, 2); }

Point* paren_init_new() { return new Point(1, 2); }

Point* paren_init_new_array() { return new Point[2](Point(1, 2), Point(3)); }

struct MemberInit {
  Point p;
  MemberInit() : p(1, 2) {}
};

struct BaseInit : Point {
  BaseInit() : Point(1, 2) {}
};
