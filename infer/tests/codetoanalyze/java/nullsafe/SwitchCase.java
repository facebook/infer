/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe;

import javax.annotation.Nullable;

public class SwitchCase {
  public String switchOnNullIsBad() {
    Color color = null;
    switch (color) {
      case BLACK:
        return "BLACK";
      case WHITE:
        return "WHITE";
      default:
        // the default case will never be called.
        // instead, an NPE will be thrown.
        return "DEFAULT";
    }
  }

  public String switchOnNullableIsBad() {
    Color color = getNullableColor();
    switch (color) {
      case BLACK:
        return "BLACK";
      case WHITE:
        return "WHITE";
      default:
        // in case `color` is null, this WON'T BE called.
        // instead, an NPE will be thrown
        return "DEFAULT";
    }
  }

  public String switchOnNonNullableIsOK() {
    Color color = getNonNullableColor();
    switch (color) {
      case BLACK:
        return "BLACK";
      case WHITE:
        return "WHITE";
      default:
        // default case won't happen, but this is fine
        return "DEFAULT";
    }
  }

  private @Nullable Color getNullableColor() {
    return Color.BLACK;
  }

  private Color getNonNullableColor() {
    return Color.BLACK;
  }
}

enum Color {
  BLACK,
  WHITE;
}
