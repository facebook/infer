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

public class BufferedReader extends Reader {

    private Reader in;

    public BufferedReader(Reader in, int sz) {
        this.in = in;
    }

    public BufferedReader(Reader in) {
        this.in = in;
    }

    public void close() throws IOException {
        if (in instanceof InputStreamReader) {
            ((InputStreamReader) in).close();
        } else if (in instanceof BufferedReader) {
            ((BufferedReader) in).close();
        } else {
            in.close();
        }
    }

    public int read() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(char cbuf[]) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(char[] cbuf, int off, int len) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public String readLine() throws IOException {
        return InferUndefined.can_throw_ioexception_string();
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
