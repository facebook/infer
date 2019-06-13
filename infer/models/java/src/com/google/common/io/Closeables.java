/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.google.common.io;

import com.facebook.infer.builtins.InferCloseables;
import com.facebook.infer.builtins.InferUndefined;
import java.io.Closeable;
import java.io.IOException;

public final class Closeables {

  public static void close(Closeable closeable, boolean swallowIOException) throws IOException {
    InferCloseables.close(closeable);
    if (!swallowIOException) InferUndefined.can_throw_ioexception_void();
  }

  public static void closeQuietly(Closeable closeable) {
    InferCloseables.closeQuietly(closeable);
  }
}
