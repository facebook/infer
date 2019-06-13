/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.io;

import com.facebook.infer.builtins.InferUndefined;

public class ObjectInputStream {

  InputStream in;

  public ObjectInputStream(InputStream in) throws IOException {
    InferUndefined.can_throw_ioexception_void();
    this.in = in;
  }

  public void close() throws IOException {
    if (in != null) {
      in.close();
    }
  }

  protected ObjectInputStream() throws IOException, SecurityException {}

  public int available() throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public void defaultReadObject() throws IOException {
    InferUndefined.can_throw_ioexception_void();
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

  public boolean readBoolean() throws IOException {
    return InferUndefined.can_throw_ioexception_boolean();
  }

  public byte readByte() throws IOException {
    return InferUndefined.can_throw_ioexception_byte();
  }

  public char readChar() throws IOException {
    return InferUndefined.can_throw_ioexception_char();
  }

  public double readDouble() throws IOException {
    return InferUndefined.can_throw_ioexception_double();
  }

  public ObjectInputStream.GetField readFields() throws IOException {
    throw new IOException();
  }

  public float readFloat() throws IOException {
    return InferUndefined.can_throw_ioexception_float();
  }

  public void readFully(byte[] buf) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void readFully(byte[] buf, int off, int len) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public int readInt() throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public long readLong() throws IOException {
    return InferUndefined.can_throw_ioexception_long();
  }

  public final Object readObject() throws IOException {
    return InferUndefined.can_throw_ioexception_object();
  }

  public short readShort() throws IOException {
    return InferUndefined.can_throw_ioexception_short();
  }

  public Object readUnshared() throws IOException, ClassNotFoundException {
    return InferUndefined.can_throw_ioexception_object();
  }

  public int readUnsignedByte() throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public int readUnsignedShort() throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public String readUTF() throws IOException {
    return InferUndefined.can_throw_ioexception_string();
  }

  public int skipBytes(int len) throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public abstract static class GetField {}
}
