/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

interface I {
  default Object defaultMethod1() {
    return null;
  }

  default Object defaultMethod2() {
    return "foo";
  }
}

public class DefaultInInterface implements I {

  public void bad() {
    System.out.println(this.defaultMethod1().toString());
  }

  public void ok() {
    System.out.println(this.defaultMethod2().toString());
  }
}
