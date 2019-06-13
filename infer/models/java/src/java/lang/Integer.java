/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.lang;

import javax.annotation.Nullable;

public final class Integer {

  public static int MAX_VALUE = 2147483647; // 2**31-1
  public static int MIN_VALUE = -2147483648; // -2**31

  protected final int value;

  public Integer(int i) {
    this.value = i;
  }

  public static Integer valueOf(int i) {
    return new Integer(i);
  }

  public boolean equals(@Nullable Object anObject) {
    return anObject != null
        && anObject instanceof Integer
        && this.value == ((Integer) anObject).value;
  }

  public int intValue() {
    return this.value;
  }
}
