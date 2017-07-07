/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
void initialized_no_dangling_ok() {
  int* p = new int(42);
  delete p;
}

void uninitialized_dangling_bad() {
  int* p;
  delete p;
}

struct TestDangling {
  int* p;
  // Field p is deliberately left uninitialized
  TestDangling() {}
  // This constructor is deliberately left unimplemented
  // to make sure Infer treats undefined function conservatively
  TestDangling(int);
};

int known_ctor_dangling_bad() {
  auto t0 = new TestDangling();
  int ret = *(t0->p); // should report a dangling pointer dereference here as p
                      // is dangling
  delete t0;
  return ret;
}

int unknown_ctor_assume_no_dangling_ok() {
  auto t1 = new TestDangling(0);
  int ret =
      *(t1->p); // TestDangling(0) is not known so p may have been initialized
  delete t1;
  return ret;
}
