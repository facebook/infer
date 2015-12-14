/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.checkers;

import com.facebook.infer.annotation.Expensive;
import com.facebook.infer.annotation.PerformanceCritical;

interface I {
  void foo();
}

class A implements I {

  @SuppressWarnings("infer") // In case we really want to implement foo as expensive
  @Expensive
  public void foo() {}

}

class B extends A implements I {
  public void foo() {}
}

public class ExpensiveInheritanceExample {

  @PerformanceCritical
  void shouldNotReportBecauseInterfaceNotAnnotated(I i) {
    i.foo();
  }

  @PerformanceCritical
  void reportsBecauseFooIsExpensiveInA() {
    A a = new A();
    a.foo();
  }

  @PerformanceCritical
  void doesNotreportBecauseFooIsNotExpensiveInB() {
    A a = new B();
    a.foo();
  }

  A actuallyReturnsObjectOfTypeB() {
    return new B();
  }

  @PerformanceCritical
  void reportsAssumingObjectOfTypeA() {
    A a = actuallyReturnsObjectOfTypeB();
    a.foo();
  }

}
