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


import java.nio.FileChannelImpl;
import java.nio.channels.FileChannel;

public class FileInputStream extends InputStream {

    private FileDescriptor fd;
    private FileChannel channel;

    private void init() {
        InferBuiltins.__set_file_attribute(this);
    }

    public FileInputStream(String name) throws FileNotFoundException {
        if (InferUndefined.boolean_undefined()) {
            init();
        } else {
            throw new FileNotFoundException();
        }
    }

    public FileInputStream(File file) throws FileNotFoundException {
        if (InferUndefined.boolean_undefined()) {
            init();
        } else {
            throw new FileNotFoundException();
        }
    }

    public FileInputStream(FileDescriptor fdObj) {
        init();
    }

    public void close() throws IOException {
        super.close();
    }

    public FileChannel getChannel() {
        channel = new FileChannelImpl(this, fd, InferUndefined.int_undefined());
        return channel;
    }

    public int available() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    @Override
    public int read() throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    @Override
    public int read(byte b[]) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    @Override
    public int read(byte b[], int off, int len) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public long skip(int n) throws IOException {
        return InferUndefined.can_throw_ioexception_long();
    }

}
