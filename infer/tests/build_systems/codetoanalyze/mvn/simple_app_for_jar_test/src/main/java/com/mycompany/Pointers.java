/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package hello;

public class Pointers {

  public static class A {
    public void method() {}
  }

  public static A mayReturnNull(int i) {
    if (i > 0) {
      return new A();
    }
    return null;
  }
}
