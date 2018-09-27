/*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.infer.builtins;

import java.io.Closeable;

public final class InferCloseables {

  private InferCloseables() {}

  public static void close(Closeable closeable) {
    if (closeable != null) {
      InferBuiltins.__set_mem_attribute(closeable);
    }
  }

  public static void closeQuietly(Closeable closeable) {
    close(closeable);
  }
}
