/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package hello;

public class Pointers {

  public static class A {
    public void method() {
    }
  }

  public static A mayReturnNull(int i) {
    if (i > 0) {
      return new A();
    }
    return null;
  }

}
