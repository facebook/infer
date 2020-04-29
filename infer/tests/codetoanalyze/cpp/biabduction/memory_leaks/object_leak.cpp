/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Rectangle {
  int width, height;

 public:
  Rectangle(int x, int y) : width(x), height(y) {}
  int area(void) { return width * height; }
};

void object_leak() { Rectangle* bar = new Rectangle(5, 6); }
