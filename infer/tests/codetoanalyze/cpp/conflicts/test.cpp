/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
