/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.util.jar;

import java.io.File;
import java.io.IOException;
import java.util.zip.ZipFile;


public class JarFile extends ZipFile {


    public JarFile(String name) throws IOException {
        super(name);
    }

    public JarFile(String name, boolean verify) throws IOException {
        super(name);
    }

    public JarFile(File file) throws IOException {
        super("");
    }

    public JarFile(File file, boolean verify) throws IOException {
        this(file, verify, 0);
    }

    public JarFile(File file, boolean verify, int mode) throws IOException {
        super("");
    }

    public Manifest getManifest() throws IOException {
        throw new IOException();
    }

    public void close() throws IOException {
        super.close();
    }

}
