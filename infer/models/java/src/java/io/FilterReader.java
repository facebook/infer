/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.io;

import com.facebook.infer.builtins.InferUndefined;

public abstract class FilterReader extends Reader {

    protected Reader in;

    protected FilterReader(Reader in) {
        this.in = in;
    }

    public void close() throws IOException {
        if (in instanceof InputStreamReader)
            ((InputStreamReader) in).close();
    }

    public int read() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(char cbuf[]) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(char cbuf[], int off, int len) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public boolean ready() throws IOException {
        return InferUndefined.can_throw_ioexception_boolean();
    }

    public void reset() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public long skip(long n) throws IOException {
        return InferUndefined.can_throw_ioexception_long();
    }

}
