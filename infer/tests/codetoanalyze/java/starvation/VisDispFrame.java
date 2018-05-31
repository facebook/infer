/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
