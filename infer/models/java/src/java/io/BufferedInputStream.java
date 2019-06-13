/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.io;

import com.facebook.infer.builtins.InferUndefined;

public class BufferedInputStream {

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

  public void reset() throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public long skip(long n) throws IOException {
    return InferUndefined.can_throw_ioexception_long();
  }
}
