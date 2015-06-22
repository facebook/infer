/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package java.util.zip;

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import com.facebook.infer.models.InferUndefined;


public class ZipOutputStream extends DeflaterOutputStream {

    public ZipOutputStream(OutputStream out) {
        super(out);
    }

    public void putNextEntry(ZipEntry e) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void closeEntry() throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public void close() throws IOException {
        if (out != null) {
            if (out instanceof FileOutputStream) {
                ((FileOutputStream) out).close();
            } else if (out instanceof BufferedOutputStream) {
                ((BufferedOutputStream) out).close();
            }
        }
    }
}
