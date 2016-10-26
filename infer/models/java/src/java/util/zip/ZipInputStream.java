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

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

public class ZipInputStream extends InflaterInputStream {

    private ZipEntry currentEntry;

    public ZipInputStream(InputStream in) {
        super(in);
    }

    public ZipEntry getNextEntry() throws IOException {
        boolean undef = InferUndefined.boolean_undefined();
        if (undef) {
            return currentEntry;
        } else
            throw new IOException();
    }

    public void closeEntry() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void close() throws IOException {
        if (in != null)
            if (in instanceof FileInputStream) {
                ((FileInputStream) in).close();
            } else if (in instanceof BufferedInputStream) {
                ((BufferedInputStream) in).close();
            }
    }

}
