/*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package com.facebook.infer.models;

import com.squareup.okhttp.internal.StrictLineReader;

import java.io.*;

public final class InferCloseables {

    private InferCloseables() {
    }

    public static void close(Closeable closeable, boolean swallowIOException)
            throws IOException {
        if (closeable == null) return;
        if (closeable instanceof InputStream) {
            ((InputStream) closeable).close();
        } else if (closeable instanceof OutputStream) {
            ((OutputStream) closeable).close();
        } else if (closeable instanceof Reader) {
            ((Reader) closeable).close();
        } else if (closeable instanceof Writer) {
            ((Writer) closeable).close();
        } else if (closeable instanceof StrictLineReader) {
            ((StrictLineReader) closeable).close();
        }
    }

    public static void closeQuietly(Closeable closeable) {
        try {
            close(closeable, true);
        } catch (IOException e) {
        }
    }

}
