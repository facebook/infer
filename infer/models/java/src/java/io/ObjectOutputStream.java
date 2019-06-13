/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.io;

import com.facebook.infer.builtins.InferUndefined;

public class ObjectOutputStream extends OutputStream {

  private DataOutputStream output;

  public ObjectOutputStream(OutputStream out) throws IOException {
    this.output = new DataOutputStream(out);
    InferUndefined.can_throw_ioexception_void();
  }

  public void close() throws IOException {
    output.close();
  }

  public void defaultWriteObject() throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void flush() throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void reset() throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void write(byte b[]) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void write(byte b[], int off, int len) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void write(int b) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void writeBoolean(boolean val) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void writeByte(int val) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void writeBytes(String str) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void writeChar(int val) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void writeChars(String str) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void writeDouble(double val) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void writeFields() throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void writeFloat(float val) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void writeInt(int val) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void writeLong(long val) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public final void writeObject(Object obj) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void writeShort(int val) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void writeUnshared(Object obj) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void writeUTF(String str) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }
}
