/*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*/

package java.util.zip;

import com.facebook.infer.models.InferBuiltins;
import com.facebook.infer.models.InferUndefined;
import dalvik.system.CloseGuard;

import java.io.*;
import java.util.Enumeration;
import java.util.LinkedHashMap;
import java.util.NoSuchElementException;

public class ZipFile {


    public ZipFile(String name) throws IOException {
        InferUndefined.can_throw_ioexception_void();
        //Had to throw before setting attribute else
        // whenInferRunsOnJarFileClosedThenResourceLeakIsNotFound fails
        InferBuiltins.__set_file_attribute(this);
    }

    public ZipFile(File file, int mode) throws IOException {
        InferUndefined.can_throw_ioexception_void();
        InferBuiltins.__set_file_attribute(this);
    }

    public ZipFile(File file) throws ZipException, IOException {
        this(file, 0);
    }

    public InputStream getInputStream(ZipEntry entry) throws IOException {
        FileInputStream in = new FileInputStream("");
        return new InflaterInputStream(in, null, 0) {
            private boolean isClosed = false;

            public void close() throws IOException {
                super.close();
            }

            protected void fill() throws IOException {
            }

            private boolean eof;

            public int available() throws IOException {
                return InferUndefined.can_throw_ioexception_int();
            }
        };
    }

    public void close() throws IOException {
        InferBuiltins.__set_mem_attribute(this);
        InferUndefined.can_throw_ioexception_void();
    }

    protected void finalize() throws IOException {
        close();
    }

    public Enumeration<? extends ZipEntry> entries() {

        return new Enumeration<ZipEntry>() {
            private boolean hasEls;

            public boolean hasMoreElements() {
                return hasEls;
            }

            public ZipEntry nextElement() throws NoSuchElementException {
                if (hasEls)
                    return new ZipEntry("");
                else
                    throw new NoSuchElementException();
            }
        };
    }
}
