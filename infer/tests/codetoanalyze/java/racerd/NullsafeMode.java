/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import com.facebook.infer.annotation.Nullsafe;
import javax.annotation.Nullable;
import javax.annotation.concurrent.GuardedBy;
import javax.annotation.concurrent.ThreadSafe;

@Nullsafe(Nullsafe.Mode.LOCAL)
@ThreadSafe
public class NullsafeMode {
  private @Nullable Object nullableField;

  public void resetBad() {
    this.nullableField = null;
  }

  public String getStringBad() {
    if (this.nullableField != null) {
      return nullableField.toString();
    } else {
      return "";
    }
  }

  @GuardedBy("this")
  private @Nullable Object nullableGuardedField;

  public void unlockedWriteBad() {
    nullableGuardedField = null;
  }
}
