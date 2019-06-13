/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.security;

import com.facebook.infer.builtins.InferUndefined;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

public class DigestOutputStream extends FilterOutputStream {

  public DigestOutputStream(OutputStream stream, MessageDigest digest) {
    super(stream);
  }

  public void write(byte b[], int off, int len) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void write(byte b[]) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void write(int b) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }
}
