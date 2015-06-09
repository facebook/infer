/*
 *  Licensed to the Apache Software Foundation (ASF) under one or more
 *  contributor license agreements.  See the NOTICE file distributed with
 *  this work for additional information regarding copyright ownership.
 *  The ASF licenses this file to You under the Apache License, Version 2.0
 *  (the "License"); you may not use this file except in compliance with
 *  the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package java.nio;

import com.facebook.infer.models.InferUndefined;

import java.io.*;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;
import java.util.Comparator;
import java.util.SortedSet;

public class FileChannelImpl extends FileChannel {
    private static Comparator<FileLock> LOCK_COMPARATOR;

    private Object stream;
    private FileDescriptor fd;
    private int mode;

    private SortedSet<FileLock> locks;

    public FileChannelImpl(Object stream, FileDescriptor fd, int mode) {
        this.fd = fd;
        this.stream = stream;
        this.mode = mode;
    }

    public void implCloseChannel() throws IOException {
        if (stream instanceof FileInputStream) {
            ((FileInputStream) stream).close();
        } else if (stream instanceof FileOutputStream) {
            ((FileOutputStream) stream).close();
        } else if (stream instanceof RandomAccessFile) {
            ((RandomAccessFile) stream).close();
        }
    }

    public long position() throws IOException {
        return InferUndefined.can_throw_ioexception_long();
    }

    public FileChannel position(long newPosition) throws IOException {
        if (InferUndefined.boolean_undefined())
            throw new IOException();
        else return this;
    }

    public int read(ByteBuffer buffer, long position) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int read(ByteBuffer buffer) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public long read(ByteBuffer[] buffers, int offset, int length) throws IOException {
        return InferUndefined.can_throw_ioexception_long();
    }

    public long size() throws IOException {
        return InferUndefined.can_throw_ioexception_long();
    }

    public long transferFrom(ReadableByteChannel src, long position, long count) throws IOException {
        return InferUndefined.can_throw_ioexception_long();
    }

    public long transferTo(long position, long count, WritableByteChannel target) throws IOException {
        return InferUndefined.can_throw_ioexception_long();
    }

    public FileChannel truncate(long size) throws IOException {
        if (InferUndefined.boolean_undefined())
            throw new IOException();
        else return this;
    }

    public int write(ByteBuffer buffer, long position) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public int write(ByteBuffer buffer) throws IOException {
        return InferUndefined.can_throw_ioexception_int();
    }

    public long write(ByteBuffer[] buffers, int offset, int length) throws IOException {
        return InferUndefined.can_throw_ioexception_long();
    }

    public FileDescriptor getFD() {
        return fd;
    }

}
