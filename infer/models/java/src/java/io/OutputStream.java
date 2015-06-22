/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package java.io;

import com.facebook.infer.models.InferUndefined;

public abstract class OutputStream implements Closeable {

    public void close() throws IOException {
        if (this instanceof FileOutputStream) {
            ((FileOutputStream) this).close();
        } else if (this instanceof FilterOutputStream) {
            ((FilterOutputStream) this).close();
        }
    }

    public void write(int b) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(byte b[]) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void write(byte b[], int off, int len) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void flush() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }
}
