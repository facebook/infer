/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package java;

import dalvik.system.BlockGuard;

public class GuardTest {
  public void testWriteBad() {
    BlockGuard.getThreadPolicy().onWriteToDisk();
  }

  public void testReadBad() {
    BlockGuard.getThreadPolicy().onReadFromDisk();
  }

  public void testNetBad() {
    BlockGuard.getThreadPolicy().onNetwork();
  }

  private void privateOk() {
    testReadBad();
  }

  public void intraprocBad() {
    privateOk();
  }

}
