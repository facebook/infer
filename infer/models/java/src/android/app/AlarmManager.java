/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package android.app;

import android.app.PendingIntent;

public abstract class AlarmManager {

  void cancel(PendingIntent operation) {
    operation.cancel();
  }
}
