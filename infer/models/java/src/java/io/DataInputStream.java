/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.io;

import com.facebook.infer.builtins.InferUndefined;

public class DataInputStream {

  public int read(byte b[]) throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public int read(byte b[], int off, int len) throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public final boolean readBoolean() throws IOException {
    return InferUndefined.can_throw_ioexception_boolean();
  }

  public final byte readByte() throws IOException {
    return InferUndefined.can_throw_ioexception_byte();
  }

  public final char readChar() throws IOException {
    return InferUndefined.can_throw_ioexception_char();
  }

  public final double readDouble() throws IOException {
    return InferUndefined.can_throw_ioexception_double();
  }

  public final float readFloat() throws IOException {
    return InferUndefined.can_throw_ioexception_float();
  }

  public final void readFully(byte b[]) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public final void readFully(byte b[], int off, int len) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public final int readInt() throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public final long readLong() throws IOException {
    return InferUndefined.can_throw_ioexception_long();
  }

  public final short readShort() throws IOException {
    return InferUndefined.can_throw_ioexception_short();
  }

  public final int readUnsignedByte() throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public final int readUnsignedShort() throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public final String readUTF() throws IOException {
    return InferUndefined.can_throw_ioexception_string();
  }

  public static final String readUTF(DataInput in) throws IOException {
    return InferUndefined.can_throw_ioexception_string();
  }

  public final int skipBytes(int n) throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }
}
