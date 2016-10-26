/*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package com.facebook.infer.builtins;

import java.io.Closeable;

public final class InferCloseables {

  private InferCloseables() {
  }

  public static void close(Closeable closeable) {
    if (closeable != null) {
      InferBuiltins.__set_mem_attribute(closeable);
    }
  }

  public static void closeQuietly(Closeable closeable) {
    close(closeable);
  }

}
