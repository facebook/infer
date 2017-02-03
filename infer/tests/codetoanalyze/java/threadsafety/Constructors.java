/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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

  public static synchronized Constructors singletonOk() {
    // ok because lock is held during write to static field in constructor
    return new Constructors(new Object());
  }

  public static Constructors singletonBad() {
    // not ok because no lock is held
    return new Constructors(new Object());
  }


}
