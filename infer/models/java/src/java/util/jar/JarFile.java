/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package java.util.jar;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.ZipEntry;
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

    public synchronized InputStream getInputStream(ZipEntry ze)
            throws IOException {
        return super.getInputStream(ze);
    }

    public void close() throws IOException {
        super.close();
    }

}
