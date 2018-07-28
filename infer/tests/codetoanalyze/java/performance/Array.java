/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.java.performance;
public class Array {

  public void array_access_good() {
    float[] radii = new float[8];
    for (int i = 0; i < 4; ++i) {
      radii[i * 2] = radii[i];
      radii[i * 2 + 1] = radii[i] + 1;
    }
  }

  public void array_access_overrun_bad() {
    float[] radii = new float[8];
    for (int i = 0; i < 4; ++i) {
      radii[i * 2] = radii[i];
      radii[i * 2 + 2] = radii[i] + 1;
    }
  }
}
