/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <memory>

namespace npe_added_to_b1 {

int deref_ref(std::shared_ptr<int>& p) { return *p; }

int causes_npe() {
  std::shared_ptr<int> x;
  return deref_ref(x);
}

class Person {
 public:
  Person() { f1 = nullptr; }
  int* f1;
};

int deref_person(Person& p) { return *(p.f1); }

int causes_npe_person() {
  Person p;
  return deref_person(p);
}
}
