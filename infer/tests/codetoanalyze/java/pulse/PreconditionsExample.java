/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.java.infer;

import com.google.common.base.Preconditions;

public class PreconditionsExample {

  Integer i1;

  /*
   * Note: Although the Guava checkNotNull method throws a NullPointerException when a given
   * value x is null, Pulse will simply try to go ahead assuming that x != null.
   * This means that it will not report NPEs on values passed to checkNotNull.
   * The analogous approach has been taken for checkState and checkArgument.
   */

  void testCheckNotNullOnNullValueOk() {
    PreconditionsExample x = null;
    // This should not be reported due to the use of checkNotNull
    Preconditions.checkNotNull(x);
    Integer i1 = x.i1;
  }

  void testCheckNotNullOnNonNullValueOk() {
    PreconditionsExample x = new PreconditionsExample();
    x.i1 = new Integer(10);
    PreconditionsExample y = Preconditions.checkNotNull(x);
    // This should not cause bug, as y.i1 should be equal to x.i1
    if (!y.i1.equals(10)) {
      Object o = null;
      o.toString();
    }
  }

  void testCheckNotNullPassingFurtherArgsOk() {
    Object x = null;
    Preconditions.checkNotNull(x, "errorMsg");
    x.toString();
  }

  void testCheckNotNullOtherDerefOk() {
    Object x = null;
    Object y = null;
    Preconditions.checkNotNull(x, "errorMsg");
    y.toString();
  }

  void testCheckNotNullArgOk(Object x) {
    Object y = null;
    Preconditions.checkNotNull(x);
    x.toString();
  }

  void testCheckNotNullArgLatent(Object x) {
    Object y = null;
    Preconditions.checkNotNull(x);
    y.toString();
  }

  void checkStateConditionSatBad() {
    PreconditionsExample x = new PreconditionsExample();
    PreconditionsExample y = null;
    x.i1 = 1000;
    Preconditions.checkState(x.i1.equals(1000));
    y.toString();
  }

  void checkStateConditionUnsatOk() {
    PreconditionsExample x = new PreconditionsExample();
    PreconditionsExample y = null;
    x.i1 = 1000;
    Preconditions.checkState(x.i1.equals(100), "errorMsg");
    y.toString();
  }

  void checkArgumentUnsatOk() {
    int age = -18;
    Object x = null;
    Preconditions.checkArgument(age > 0);
    x.getClass();
  }

  void checkAgeOver18(int age) {
    Preconditions.checkArgument(age > 18, "errorMsg");
  }

  void checkArgumentSatBad() {
    int age = 20;
    Object x = null;
    checkAgeOver18(age);
    x.getClass();
  }
}
