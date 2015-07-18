/*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*/

package java.io;

import com.facebook.infer.models.InferUndefined;

public class BufferedInputStream extends FilterInputStream {

    public BufferedInputStream(InputStream in) {
        super(in);
    }

    public BufferedInputStream(InputStream in, int size) {
        super(in);
    }

    public int available() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public void close() throws IOException {
        if (in != null) {
            in.close();
        }
    }

    public int read() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(byte b[]) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(byte b[], int off, int len) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public void reset() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public long skip(long n) throws IOException {
        return InferUndefined.can_throw_ioexception_long();
    }
}
