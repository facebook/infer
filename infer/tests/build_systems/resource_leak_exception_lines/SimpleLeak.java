/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import java.io.*;

public class SimpleLeak {
  // TODO: pulse should point out where the exception is raised to understand the issue
  public void testExceptionGetsFlagged_Not(boolean v) {
    try {
      FileInputStream a = new FileInputStream("aaa");
      FileInputStream z = null;
      if (v) {
        z = new FileInputStream("bbb");
        z.read();
      }
      z.close();
    } catch (IOException e) {
      // do nothing
    }
  }
}
