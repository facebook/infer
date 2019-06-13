/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.java.checkers;

import com.google.common.base.Preconditions;
import javax.annotation.Nullable;

public class NullableViolation {

  class T {
    int x;

    void doSomething() {}
  }

  static native @Nullable T returnsNullable();

  void dereferenceNullableReturnValueBad() {
    T t = returnsNullable();
    t.x = 42; // reports here
  }

  void dereferenceNullableReturnValueOkay() {
    T t = returnsNullable();
    if (t != null) {
      t.x = 42; // does not report here
    }
  }

  void dereferenceNullableMethodBad() {
    returnsNullable().doSomething(); // reports here
  }

  void dereferenceNullableMethodCheckedForNullOkay() {
    if (returnsNullable() != null) {
      returnsNullable().doSomething(); // does not report here
    }
  }

  void nullableMethodCheckedForNullAndReturnOkay() {
    if (returnsNullable() == null) {
      return;
    }
    returnsNullable().doSomething(); // does not report here
  }

  void dereferenceNullableMethodIncorrectlyCheckedForNullBad() {
    if (returnsNullable() == null) {
      returnsNullable().doSomething(); // reports here
    }
  }

  void dereferenceNullableMethodInElseBranchBad() {
    if (returnsNullable() != null) {
    } else {
      returnsNullable().doSomething(); // reports here
    }
  }

  native boolean star();

  void dereferenceNullableMethodAlwaysCheckedForNullOkay() {
    if (star() && returnsNullable() != null) {
      returnsNullable().doSomething(); // does not report here
    }
  }

  void dereferenceNullableMethodNotAlwaysCheckedForNullBad() {
    if (star() || returnsNullable() != null) {
      returnsNullable().doSomething(); // reports here
    }
  }

  void usePreconditionsCheckNotNullOnVariableOkay() {
    T t = returnsNullable();
    Preconditions.checkNotNull(t);
    t.doSomething(); // does not report here
  }

  void usePreconditionsCheckNotNullOnMethodOkay() {
    Preconditions.checkNotNull(returnsNullable()).doSomething(); // does not report here
  }

  void usePreconditionsCheckNotNullRepeatedCallOkay() {
    Preconditions.checkNotNull(returnsNullable());
    returnsNullable().doSomething(); // does not report here
  }

  native @Nullable Object getNullableObject();

  void pointerAssignmentWithSubtype() {
    Object object = getNullableObject();
    object = "Hello";
  }

  void deferenceAliasOfNullableValueCheckedForNullOkay() {
    T t = returnsNullable();
    T s = t;
    if (t != null) {
      s.x = 42;
    }
  }

  void dereferenceWithAssignmentExpressionsOkay() {
    T t;
    while ((t = returnsNullable()) != null) {
      t.doSomething();
    }
  }
}
