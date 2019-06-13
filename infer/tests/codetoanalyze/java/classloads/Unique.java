/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Unique {
  public static void main(String args[]) {
    // two loads for the same class with distinct locations and traces should be merged into one
    UniqueObj u = new UniqueObj();
    u.foo();
  }
}

class UniqueObj {
  void foo() {}
}
