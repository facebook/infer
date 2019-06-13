/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Prune {
  public static void main(String args[]) {
    if (PruneA.f == 0) {
      System.out.println(PruneB.g < 0);
    }
  }
}

class PruneA {
  static int f;
}

class PruneB {
  static int g;
}
