/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package java.util.zip;

import com.facebook.infer.models.InferUndefined;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

public class ZipInputStream extends InflaterInputStream {

    private ZipEntry currentEntry;

    public ZipInputStream(InputStream in) {
        super(in);
    }

    public ZipEntry getNextEntry() throws IOException {
        boolean undef = InferUndefined.boolean_undefined();
        if (undef) {
            return currentEntry;
        } else
            throw new IOException();
    }

    public void closeEntry() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void close() throws IOException {
        if (in != null)
            if (in instanceof FileInputStream) {
                ((FileInputStream) in).close();
            } else if (in instanceof BufferedInputStream) {
                ((BufferedInputStream) in).close();
            }
    }

}
