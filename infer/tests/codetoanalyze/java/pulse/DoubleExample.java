/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

import javax.annotation.Nullable;

public class DoubleExample {

  @Nullable Double x;

  private Double testAssignNonNullOk() {
    x = 1.0;
    return x + 1.0;
  }

  private Double FN_testdReadNullableBad() {
    return x + 1.0;
  }
}
