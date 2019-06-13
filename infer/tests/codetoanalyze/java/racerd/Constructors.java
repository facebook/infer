/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import javax.annotation.concurrent.ThreadSafe;

@ThreadSafe
public class Constructors {
  int field;
  static Object staticField;

  public Constructors(int i) {
    field = i; // ok
  }

  public Constructors() {
    staticField = new Object(); // not ok;
  }

  private Constructors(Object o) {
    staticField = o; // ok because this is private
  }

  public Constructors(Constructors o) {
    o.field = 42; // not ok
  }

  public Constructors(String s) {
    calledFromConstructorOk(); // ok
  }

  private void calledFromConstructorOk() {
    this.field = 7;
  }

  public static synchronized Constructors singleton1Ok() {
    // ok because lock is held during write to static field in constructor
    return new Constructors(new Object());
  }

  private static Constructors sSingleton1;

  public static Constructors FP_singleton2Ok() {
    synchronized (Constructors.class) {
      if (sSingleton1 != null) {
        sSingleton1 = new Constructors(0);
      }
    }
    return sSingleton1; // not currently smart enough to understand that this read is ok
  }

  public static Constructors singleton1Bad() {
    // not ok because no lock is held
    return new Constructors(new Object());
  }

  private static Constructors sSingleton2;

  public static Constructors singleton2Bad() {
    if (sSingleton2 == null) {
      sSingleton2 = new Constructors(0);
    }
    return sSingleton2;
  }
}
