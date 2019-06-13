/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.io;

import com.facebook.infer.builtins.InferUndefined;

public abstract class PushbackReader {

  public int read() throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public int read(char cbuf[]) throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public int read(char cbuf[], int off, int len) throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public boolean ready() throws IOException {
    return InferUndefined.can_throw_ioexception_boolean();
  }

  public void reset() throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public long skip(long n) throws IOException {
    return InferUndefined.can_throw_ioexception_long();
  }

  public void unread(char cbuf[]) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void unread(char cbuf[], int off, int len) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void unread(int c) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }
}
