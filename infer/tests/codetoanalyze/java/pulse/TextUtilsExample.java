/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import android.text.TextUtils;

public class TextUtilsExample {

  public void testTextUtilsIsEmptyLatent(String s) {
    if (TextUtils.isEmpty(s)) {
      Object o = null;
      o.toString();
    }
  }

  public void testTextUtilsIsEmptyBad() {
    String s = "#@%^&%";
    if (!TextUtils.isEmpty(s)) {
      Object o = null;
      o.toString();
    }
  }

  public void testTextUtilsIsEmptyEmptyStrBad() {
    if (TextUtils.isEmpty("")) {
      Object o = null;
      o.toString();
    }
  }

  public void testTextUtilsIsEmptyNullBad() {
    String s = null;
    if (TextUtils.isEmpty(s)) {
      Object o = null;
      o.toString();
    }
  }
}
