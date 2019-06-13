/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.java.bufferoverrun;

public class ArrayMember {
  public int[] buf;

  public void load_array_member_Good() {
    int[] a = new int[10];
    int x = buf[0];
    if (x == 9) {
      a[x] = 0;
    }
  }

  public void load_array_member_Bad() {
    int[] a = new int[10];
    int x = buf[0];
    if (x == 10) {
      a[x] = 0;
    }
  }
}
