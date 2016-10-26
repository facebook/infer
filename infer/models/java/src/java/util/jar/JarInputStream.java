/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.util.jar;

import com.facebook.infer.builtins.InferUndefined;

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
