/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Catch {
  public static void main(String args[]) {
    try {
      foo();
    } catch (CatchA a) {
    }
  }

  static void foo() throws CatchA {}
}

class CatchA extends Exception {
  static final long serialVersionUID = 0L;
}
