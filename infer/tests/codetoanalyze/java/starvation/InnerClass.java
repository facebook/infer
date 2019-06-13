/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class InnerClass {
  synchronized void outerInnerOk(InnerClassA a) {
    a.foo();
  }

  synchronized void bar() {}

  synchronized void outerInnerBad(InnerClassA a) {
    a.baz();
  }

  class InnerClassA {
    void foo() {
      synchronized (InnerClass.this) {
      }
    }

    void outerInnerOk() {
      synchronized (InnerClass.this) {
        InnerClass.this.bar();
      }
    }

    synchronized void baz() {}

    synchronized void innerOuterBad() {
      InnerClass.this.bar();
    }

    // ctrs generate different access paths so test these too
    // following should not be flagged
    InnerClassA() {
      synchronized (InnerClass.this) {
        InnerClass.this.bar();
      }
    }

    // following should be flagged with outer_inner_bad()
    InnerClassA(Object o) {
      synchronized (this) {
        InnerClass.this.bar();
      }
    }
  }
}
