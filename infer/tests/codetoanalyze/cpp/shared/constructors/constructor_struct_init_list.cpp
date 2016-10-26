/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

typedef struct { int top, left, bottom, right; } Insets;

struct Person {
  int age;
  Person(const Insets l) : age(l.top) {}
};

void test() { Person p({}); }
