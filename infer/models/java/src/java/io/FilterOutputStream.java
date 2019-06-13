/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.io;

import com.facebook.infer.builtins.InferUndefined;

public class FilterOutputStream {

  protected OutputStream out;

  public FilterOutputStream() {}

  public FilterOutputStream(OutputStream out) {
    this.out = out;
  }

  public void close() throws IOException {
    if (out != null) {
      out.close();
    }
  }

  public void flush() throws IOException {
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
}
