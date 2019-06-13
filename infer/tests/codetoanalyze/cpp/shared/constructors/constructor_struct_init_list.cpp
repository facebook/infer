/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef struct {
  int top, left, bottom, right;
} Insets;

struct Person {
  int age;
  Person(const Insets l) : age(l.top) {}
};

void test() { Person p({}); }
