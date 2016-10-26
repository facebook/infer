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
import com.facebook.infer.builtins.InferUtils;

import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;

public class OutputStreamWriter extends Writer {

    private OutputStream out;

    public OutputStreamWriter(OutputStream out, String charsetName)
            throws UnsupportedEncodingException {
        if (charsetName == null)
            throw new NullPointerException("charsetName");
        else if (InferUtils.isValidCharset(charsetName)) {
            this.out = out;
        } else
            throw new UnsupportedEncodingException();
    }

    public OutputStreamWriter(OutputStream out) {
        this.out = out;
    }

    public OutputStreamWriter(OutputStream out, Charset cs) {
        this.out = out;
    }

    public OutputStreamWriter(OutputStream out, CharsetEncoder enc) {
        this.out = out;
    }

    public void flush() throws IOException {
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
        out.close();
    }
}
