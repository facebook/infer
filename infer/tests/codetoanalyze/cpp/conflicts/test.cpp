/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
class C {
 private:
  int f;

 public:
  int get() { return f; }
};

C* _Nullable nullableMethod();

int test1_bad() {
  C* p = nullableMethod();
  return p->get(); // reported by the biabduction analysis
}

int test2_bad() {
  return nullableMethod()->get(); // not reported by the biabduction analysis
}

int test3_bad() {
  C* p = nullableMethod();
  for (int i = 0; i < 10; i++) {
    p = nullableMethod();
  }
  return p->get(); // reported by the biabduction analysis
}
