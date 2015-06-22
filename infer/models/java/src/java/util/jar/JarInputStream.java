/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package java.util.jar;

import com.facebook.infer.models.InferUndefined;

import java.io.IOException;
import java.io.InputStream;
import java.util.zip.ZipInputStream;


public class JarInputStream extends ZipInputStream {

    public JarInputStream(InputStream in) throws IOException {
        super(in);
        InferUndefined.can_throw_ioexception_void();
    }

    public JarInputStream(InputStream in, boolean verify) throws IOException {
        super(in);
        InferUndefined.can_throw_ioexception_void();
    }

}
