/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.nio;

import com.facebook.infer.builtins.InferUndefined;
import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.WritableByteChannel;

public class FileChannelImpl extends FileChannel {

  private Object stream;
  private FileDescriptor fd;
  private int mode;

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
    if (InferUndefined.boolean_undefined()) throw new IOException();
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
    if (InferUndefined.boolean_undefined()) throw new IOException();
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
