/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

public class Assertion {

  public void assertFalseBad() {
    assert false;
  }

  public void assertTrueOk() {
    assert true;
  }

  /* the assert-error issue is not latent: we consider such a
  partial function should be considered bad */
  public void runAssertBooleanLatentBad(boolean b) {
    assert b;
  }

  /* the assert-error issue is not latent: we consider such a
  partial function should be considered bad */
  public void runAssertIntLatentBad(int i) {
    assert (i > 0);
  }
}
