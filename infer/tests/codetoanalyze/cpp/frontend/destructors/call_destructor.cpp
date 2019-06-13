/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct Person {
  int age;
  ~Person();
};

void f(Person* p) { p->Person::~Person(); }
