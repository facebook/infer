/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package java.util.zip;

import com.facebook.infer.models.InferUndefined;

import java.io.IOException;
import java.io.OutputStream;

public class GZIPOutputStream extends DeflaterOutputStream {


    public GZIPOutputStream(OutputStream out, int size) throws IOException {
        super(out);
        if (!InferUndefined.boolean_undefined()) {
            throw new IOException();
        }
    }

    public GZIPOutputStream(OutputStream out) throws IOException {
        super(out);
        if (!InferUndefined.boolean_undefined()) {
            throw new IOException();
        }
    }

}
