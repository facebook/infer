/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
struct B {
  B(int v) : f(v){};
  int f;
};

struct A {
  A(int f) { b = new B(f); }
  B* b;
  B* getb() { return b; };
  ~A() { delete b; }
};

struct ReferenceWrapper {
  ReferenceWrapper(A& a) : b(a.getb()){};
  B* b;
};

ReferenceWrapper getwrapper() {
  A a(1);
  return a; // We store a.b in ReferenceWrapper, but we delete a.b in the
            // destructor of A
}

int FN_reference_wrapper_bad() {
  ReferenceWrapper rw = getwrapper();
  return rw.b->f; // we want to report use after lifetime bug here
}
