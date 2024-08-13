/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

import static junit.framework.Assert.assertFalse;
import static junit.framework.Assert.assertTrue;

public class JunitAssertion {
  class A {
    public void f() {}
  }

  // need model
  public void FP_consistentAssertionOk(A a) {
    assertTrue(a != null);
    a.f();
  }

  public void inconsistentAssertionBad(A a) {
    assertFalse("Should not happen!", a != null);
    a.f();
  }
}
