/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.io;

import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;

public class PipedReader extends Reader {

  private void init() throws IOException {
    InferUndefined.can_throw_ioexception_void();
    InferBuiltins.__set_file_attribute(this);
  }

  public PipedReader() {}

  public PipedReader(int pipeSize) {}

  public PipedReader(PipedWriter src) throws IOException {
    init();
  }

  public PipedReader(PipedWriter src, int pipeSize) throws IOException {
    init();
  }

  public void connect(PipedWriter src) throws IOException {
    init();
  }

  public int read() throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public int read(char[] cbuf, int off, int len) throws IOException {
    return InferUndefined.can_throw_ioexception_int();
  }

  public boolean ready() throws IOException {
    return InferUndefined.can_throw_ioexception_boolean();
  }

  public void mark(int readAheadLimit) throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public void reset() throws IOException {
    InferUndefined.can_throw_ioexception_void();
  }

  public long skip(long n) throws IOException {
    return InferUndefined.can_throw_ioexception_long();
  }

  public void close() throws IOException {
    InferBuiltins.__set_mem_attribute(this);
    InferUndefined.can_throw_ioexception_void();
  }
}
