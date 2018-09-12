/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package dalvik.system;

public final class BlockGuard {
  public interface Policy {
    void onWriteToDisk();
    void onReadFromDisk();
    void onNetwork();
    int getPolicyMask();
  }

  public static final Policy threadPolicy = new Policy() {
      public void onWriteToDisk() {}
      public void onReadFromDisk() {}
      public void onNetwork() {}
      public int getPolicyMask() {
        return 0;
      }
    };

  public static Policy getThreadPolicy() {
    return threadPolicy;
  }
}
