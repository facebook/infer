/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package java.util.jar;

import com.facebook.infer.models.InferUndefined;

import java.io.IOException;
import java.io.OutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

public class JarOutputStream extends ZipOutputStream {

    public JarOutputStream(OutputStream out, Manifest man) throws IOException {
        super(out);
        InferUndefined.can_throw_ioexception_void();
    }

    public JarOutputStream(OutputStream out) throws IOException {
        super(out);
        InferUndefined.can_throw_ioexception_void();
    }

    public void putNextEntry(ZipEntry ze) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

}
