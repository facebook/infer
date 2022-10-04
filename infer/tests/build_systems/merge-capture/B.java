/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

public class B {

  public int bDivideByZero() {
    int denom = 0;
    int nom = 5;
    int result = nom / denom;
    return result;
  }
}
