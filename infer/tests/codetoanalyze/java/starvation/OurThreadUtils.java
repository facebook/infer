/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class OurThreadUtils {
  static native boolean isMainThread();

  static void assertMainThread() {}

  static void assertHoldsLock(Object lock) {}
}
