/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

class Rectangle {
  int width, height;

 public:
  Rectangle(int x, int y) : width(x), height(y) {}
  int area(void) { return width * height; }
};

void object_leak() { Rectangle* bar = new Rectangle(5, 6); }
