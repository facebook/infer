/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.nullsafe;

import java.io.IOException;
import java.io.StringWriter;

public class TryWithResource {
  private static final int[] KEYS = {1};

  private void FP_OK_StringWriterInTWRBlock() throws IOException {
    // IMPORTANT: this for-loop is needed for issue to manifest.
    for (int key : KEYS) {
      // no-op
    }

    try (StringWriter stringWriter = new StringWriter()) {
      // no-op
    }
  }
}
