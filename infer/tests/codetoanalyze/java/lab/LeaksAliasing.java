/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import java.io.FileInputStream;
import java.io.IOException;

public class LeaksAliasing {

  void releaseBothOk(FileInputStream stream1, FileInputStream stream2) throws IOException {
    if (stream1 == stream2) {
      stream1.close();
    } else {
      stream1.close();
      stream2.close();
    }
  }
}
