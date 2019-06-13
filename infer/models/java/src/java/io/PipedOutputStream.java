/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.io;

import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;

public class PipedOutputStream extends OutputStream {

  public PipedOutputStream(PipedInputStream snk) throws IOException {
    InferBuiltins.__set_file_attribute(this);
  }

  public PipedOutputStream() {
    InferBuiltins.__set_file_attribute(this);
  }

  public void close() throws IOException {
    InferBuiltins.__set_mem_attribute(this);
    InferUndefined.can_throw_ioexception_void();
  }

  public void connect(PipedInputStream snk) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void flush() throws IOException {
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
}
