package codetoanalyze.java.infer;

import static junit.framework.Assert.assertFalse;
import static junit.framework.Assert.assertTrue;

public class JunitAssertion {
  class A {
    public void f() {
    }
  }

  public void consistentAssumtion(A a) {
    assertTrue(a != null);
    a.f();
  }

  public void inconsistentAssertion(A a) {
    assertFalse("Should not happen!", a != null);
    a.f();
  }
}
