/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.util.zip;

import com.facebook.infer.builtins.InferUndefined;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;

public class InflaterOutputStream extends FilterOutputStream {

  public InflaterOutputStream(OutputStream out) {
    super(out);
  }

  public InflaterOutputStream(OutputStream out, Inflater infl) {
    super(out);
  }

  public InflaterOutputStream(OutputStream out, Inflater infl, int bufLen) {
    super(out);
  }

  public void close() throws IOException {
    super.close();
  }

  public void finish() throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void flush() throws IOException {
    InferUndefined.can_throw_ioexception_void();
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
