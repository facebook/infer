/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package dalvik.system;

public class CloseGuard {

  public static interface Reporter {
    public void report(String message, Throwable allocationSite);
  }
}
