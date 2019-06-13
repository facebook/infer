/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.io;

import com.facebook.infer.builtins.InferUndefined;

public abstract class Writer {

  public Writer append(char c) throws IOException {
    InferUndefined.can_throw_ioexception_void();
    return this;
  }

  public Writer append(CharSequence csq) throws IOException {
    InferUndefined.can_throw_ioexception_void();
    return this;
  }

  public Writer append(CharSequence csq, int start, int end) throws IOException {
    InferUndefined.can_throw_ioexception_void();
    return this;
  }

  public void flush() throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void write(char cbuf[]) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void write(char cbuf[], int off, int len) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void write(int c) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void write(String str) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void write(String str, int off, int len) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }
}
