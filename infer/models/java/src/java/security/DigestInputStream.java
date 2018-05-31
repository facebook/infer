/*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.security;

import com.facebook.infer.builtins.InferUndefined;

import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;


public class DigestInputStream {

  public int read() throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public int read(byte b[]) throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public int read(byte b[], int off, int len) throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

}
