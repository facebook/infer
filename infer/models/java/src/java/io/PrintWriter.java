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

public class PrintWriter extends Writer {

    protected Writer out;

    public PrintWriter(OutputStream out) {
        this(new OutputStreamWriter(out));
    }

    public PrintWriter(OutputStream out, boolean autoFlush) {
        this(new OutputStreamWriter(out), autoFlush);
    }

    public PrintWriter(Writer wr) {
        out = wr;
    }

    public PrintWriter(Writer wr, boolean autoFlush) {
        out = wr;
    }

    public PrintWriter(File file) throws FileNotFoundException {
        this(new OutputStreamWriter(new BufferedOutputStream(new FileOutputStream(file))));
    }

    public PrintWriter(File file, String csn) throws FileNotFoundException,
            UnsupportedEncodingException {
        this(new OutputStreamWriter(new BufferedOutputStream(new FileOutputStream(file))));
    }

    public PrintWriter(String fileName) throws FileNotFoundException {
        this(new OutputStreamWriter(new BufferedOutputStream(new FileOutputStream(fileName))));
    }

    public PrintWriter(String fileName, String csn)
            throws FileNotFoundException, UnsupportedEncodingException {
        this(new OutputStreamWriter(new BufferedOutputStream(new FileOutputStream(fileName))));
    }

    public PrintWriter append(char c) throws IOException {
        InferUndefined.can_throw_ioexception_void();
        return this;
    }

    public PrintWriter append(CharSequence csq) throws IOException {
        InferUndefined.can_throw_ioexception_void();
        return this;
    }

    public PrintWriter append(CharSequence csq, int start, int end)
            throws IOException {
        InferUndefined.can_throw_ioexception_void();
        return this;
    }

    public void close() {
        if (out != null) {
            try {
                if (out instanceof OutputStreamWriter) {
                    ((OutputStreamWriter) out).close();
                } else if (out instanceof BufferedWriter) {
                    ((BufferedWriter) out).close();
                }
            } catch (IOException x) {
            }
        }
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


}
