/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe_default;

import javax.annotation.Nullable;

public class NoReuseUndefFunctionValues {

  Object mObject1;
  Object mObject2;

  native Object create();

  public NoReuseUndefFunctionValues(@Nullable Object object) {
    if (object != null) {
      this.mObject1 = object;
    } else {
      this.mObject1 = this.create();
    }
    if (object != null) {
      this.mObject2 = object;
    } else {
      this.mObject2 = this.create();
    }
  }
}
