/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package com.google.common.io;

import com.facebook.infer.builtins.InferCloseables;
import com.facebook.infer.builtins.InferUndefined;

import java.io.Closeable;
import java.io.IOException;

public final class Closeables {

    public static void close(Closeable closeable, boolean swallowIOException) throws IOException {
        InferCloseables.close(closeable);
        if (!swallowIOException)
            InferUndefined.can_throw_ioexception_void();
    }

    public static void closeQuietly(Closeable closeable) {
        InferCloseables.closeQuietly(closeable);
    }

}
