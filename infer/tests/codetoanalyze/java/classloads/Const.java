/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Const {
  public static void main(String args[]) {
    synchronized (ConstA.class) {
    }

    java.lang.Class<ConstB> b = ConstB.class;
  }
}

class ConstA {}

class ConstB {}
