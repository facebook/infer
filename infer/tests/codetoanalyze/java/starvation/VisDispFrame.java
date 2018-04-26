/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

import android.view.View;
import android.graphics.Rect;
import android.support.annotation.UiThread;

class VisDispFrame {
  View view;
  Rect rect;

  @UiThread
  void callsGetVisibleDisplayFrameOnUiThreadBad() {
    view.getWindowVisibleDisplayFrame(rect);
  }

  void callsGetVisibleDisplayFrameOk() {
    view.getWindowVisibleDisplayFrame(rect);
  }
}
