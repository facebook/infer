/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package java.util.zip;

import com.facebook.infer.models.InferUndefined;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;

public class InflaterInputStream extends FilterInputStream {

    public InflaterInputStream(InputStream in, Inflater inf, int size) {
        super(in);
    }

    public InflaterInputStream(InputStream in, Inflater inf) {
        super(in);
    }

    public InflaterInputStream(InputStream in) {
        super(in);
    }

    public int available() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public void close() throws IOException {
        super.close();
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
