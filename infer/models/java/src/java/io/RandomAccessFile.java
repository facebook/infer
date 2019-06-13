/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.io;

import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;
import java.nio.FileChannelImpl;
import java.nio.channels.FileChannel;

public class RandomAccessFile implements Closeable {

  private FileDescriptor fd;
  private FileChannel channel;

  public RandomAccessFile(String name, String mode) throws FileNotFoundException {
    InferBuiltins.__set_file_attribute(this);
  }

  public RandomAccessFile(File file, String mode) throws FileNotFoundException {
    InferBuiltins.__set_file_attribute(this);
  }

  public FileChannel getChannel() {
    channel = new FileChannelImpl(this, fd, InferUndefined.int_undefined());
    return channel;
  }

  public void close() throws IOException {
    InferBuiltins.__set_mem_attribute(this);
    InferUndefined.can_throw_ioexception_void();
  }

  public int read() throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public int read(byte b[], int off, int len) throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public int read(byte b[]) throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public final void readFully(byte b[]) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public final void readFully(byte b[], int off, int len) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void write(int b) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void write(byte b[]) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void write(byte b[], int off, int len) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void seek(long pos) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public long length() throws IOException {
    return InferUndefined.can_throw_ioexception_long();
  }

  public final boolean readBoolean() throws IOException {
    return InferUndefined.can_throw_ioexception_boolean();
  }

  public final byte readByte() throws IOException {
    return InferUndefined.can_throw_ioexception_byte();
  }

  public final int readUnsignedByte() throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public final short readShort() throws IOException {
    return InferUndefined.can_throw_ioexception_short();
  }

  public final int readUnsignedShort() throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public final char readChar() throws IOException {
    return InferUndefined.can_throw_ioexception_char();
  }

  public final int readInt() throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public final long readLong() throws IOException {
    return InferUndefined.can_throw_ioexception_long();
  }

  public final float readFloat() throws IOException {
    return InferUndefined.can_throw_ioexception_float();
  }

  public final double readDouble() throws IOException {
    return InferUndefined.can_throw_ioexception_double();
  }

  public final String readLine() throws IOException {
    return InferUndefined.can_throw_ioexception_string();
  }

  public final String readUTF() throws IOException {
    return InferUndefined.can_throw_ioexception_string();
  }

  public final void writeBoolean(boolean v) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public final void writeByte(int v) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public final void writeShort(int v) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public final void writeChar(int v) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public final void writeInt(int v) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public final void writeLong(long v) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public final void writeFloat(float v) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public final void writeDouble(double v) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public final void writeBytes(String s) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public final void writeChars(String s) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public final void writeUTF(String str) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }
}
