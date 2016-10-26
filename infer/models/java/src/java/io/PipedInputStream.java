/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.io;

import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;

public class PipedInputStream extends InputStream {

    public PipedInputStream(PipedOutputStream src) throws IOException {
        this();
    }

    public PipedInputStream(PipedOutputStream src, int pipeSize)
            throws IOException {
        this();
    }

    public PipedInputStream() {
        InferBuiltins.__set_file_attribute(this);
    }

    public PipedInputStream(int pipeSize) {
        this();
    }

    public void close() throws IOException {
        InferBuiltins.__set_mem_attribute(this);
        InferUndefined.can_throw_ioexception_void();
    }

    public void connect(PipedOutputStream src) throws IOException {
        InferUndefined.can_throw_ioexception_void();
    }

    public int available() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(byte b[]) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(byte b[], int off, int len) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

}
