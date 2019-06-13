/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.io;

import com.facebook.infer.builtins.InferUndefined;

public abstract class BufferedReader {

  Reader mReader;

  public BufferedReader(Reader in) {
    mReader = in;
  }

  public BufferedReader(Reader in, int sz) {
    mReader = in;
  }

  public int read() throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public int read(char cbuf[]) throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public int read(char[] cbuf, int off, int len) throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public String readLine() throws IOException {
    return InferUndefined.can_throw_ioexception_string();
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

  public void close() {
    try {
      if (mReader != null) {
        mReader.close();
      }
    } catch (Exception e) {
      // Swallow exception
    }
  }
}
