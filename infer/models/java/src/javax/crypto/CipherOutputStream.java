/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package javax.crypto;

import com.facebook.infer.builtins.InferUndefined;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

public class CipherOutputStream extends FilterOutputStream {

  public CipherOutputStream(OutputStream os, Cipher c) {
    super(os);
  }

  protected CipherOutputStream(OutputStream os) {
    super(os);
  }

  public void close() throws IOException {
    super.close();
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

  public void flush() throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }
}
