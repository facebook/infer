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


public class BufferedWriter extends Writer {

    private Writer out;

    public BufferedWriter(Writer out) {
        this.out = out;
    }

    public BufferedWriter(Writer out, int sz) {
        this.out = out;
    }

    public void flush() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void newLine() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(char cbuf[]) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(char cbuf[], int off, int len) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(int c) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(String str) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(String str, int off, int len) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void close() throws IOException {
        if (out instanceof OutputStreamWriter) {
            ((OutputStreamWriter) out).close();
        }
    }


}
