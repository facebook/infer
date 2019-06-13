/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

using namespace std;

namespace foo {
typedef struct {
  int a;
  int b;
} my_record;
int value() { return 5; }

class Rectangle {
  int width, height;

 public:
  void set_values(int, int);
  int area(void);
};
} // namespace foo

namespace bar {
const double pi = 3.1416;
double value() { return 2 * pi; }

class Rectangle {
  int width, height;

 public:
  void set_values(int, int);
  int area(void);
} rect;
} // namespace bar

int main() {

  int i;
  double j;

  foo::my_record x;

  bar::Rectangle rect1;
  rect1.set_values(3, 4);

  foo::Rectangle rect2;
  rect2.set_values(7, 10);

  x.a = 10;
  i = foo::value();
  i = bar::value();
  j = bar::pi;
  return 0;
}
