/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.infer;

import static junit.framework.Assert.assertFalse;
import static junit.framework.Assert.assertTrue;

public class JunitAssertion {
  class A {
    public void f() {
    }
  }

  public void consistentAssertion(A a) {
    assertTrue(a != null);
    a.f();
  }

  public void inconsistentAssertion(A a) {
    assertFalse("Should not happen!", a != null);
    a.f();
  }
}
