/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.lang;

import javax.annotation.Nullable;

public final class Double {

  protected final double value;

  public Double(double i) {
    this.value = i;
  }

  public static Double valueOf(double i) {
    return new Double(i);
  }

  public boolean equals(@Nullable Object anObject) {
    return anObject != null
        && anObject instanceof Double
        && this.value == ((Double) anObject).value;
  }

  public double doubleValue() {
    return this.value;
  }
}
