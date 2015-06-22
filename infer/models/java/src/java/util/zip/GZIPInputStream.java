/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package java.util.zip;

import com.facebook.infer.models.InferUndefined;

import java.io.IOException;
import java.io.InputStream;

public class GZIPInputStream extends InflaterInputStream {


    public GZIPInputStream(InputStream in, int size) throws IOException {
        super(in);
        if (!InferUndefined.boolean_undefined()) {
            throw new IOException();
        }
    }

    public GZIPInputStream(InputStream in) throws IOException {
        super(in);
        if (!InferUndefined.boolean_undefined()) {
            throw new IOException();
        }
    }

    public void close() throws IOException {
        super.close();
    }
}
