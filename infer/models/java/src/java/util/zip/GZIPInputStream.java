/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.util.zip;

import com.facebook.infer.builtins.InferUndefined;

import java.io.IOException;
import java.io.InputStream;

public class GZIPInputStream extends InflaterInputStream {


    public GZIPInputStream(InputStream in, int size) throws IOException {
        super(in);
        if (!InferUndefined.boolean_undefined()) {
            throw new IOException();
        }
    }

    public GZIPInputStream(InputStream in) throws IOException {
        super(in);
        if (!InferUndefined.boolean_undefined()) {
            throw new IOException();
        }
    }

    public void close() throws IOException {
        super.close();
    }
}
