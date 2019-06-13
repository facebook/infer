/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import javax.annotation.concurrent.ThreadSafe;

@ThreadSafe
public class SwitchEnum {
  int[] a = new int[8];

  // Java generates a class for the switch, which contains an int array
  // This leads to races where there are int arrays, here a[]
  public String getName(EnumClass value) {
    synchronized (this) {
      a[0] = 0; // should not report here
    }
    switch (value) {
      case VALUE1:
        return "value 1";
      case VALUE3:
        return "value 3";
      default:
        return "other";
    }
  }
}

enum EnumClass {
  VALUE1,
  VALUE2,
  VALUE3
}
