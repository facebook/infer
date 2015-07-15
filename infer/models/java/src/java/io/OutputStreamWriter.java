/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package java.io;

import com.facebook.infer.models.InferUndefined;
import com.facebook.infer.models.InferUtils;

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
