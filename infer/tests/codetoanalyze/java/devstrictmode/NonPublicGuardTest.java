/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import dalvik.system.BlockGuard;

public class NonPublicGuardTest {
  public void testWriteOk() {
    BlockGuard.getThreadPolicy().onWriteToDisk();
  }

  public void testReadOk() {
    BlockGuard.getThreadPolicy().onReadFromDisk();
  }

  public void testNetOk() {
    BlockGuard.getThreadPolicy().onNetwork();
  }

  private void privateOk() {
    testReadOk();
  }

  public void intraprocOk() {
    privateOk();
  }

}
