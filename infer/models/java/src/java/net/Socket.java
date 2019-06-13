/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.net;

import com.facebook.infer.builtins.InferBuiltins;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class Socket {

  InputStream inputStream;
  OutputStream outputStream;

  public Socket() {
    InferBuiltins.__set_file_attribute(this);
    inputStream = new InputStream();
    InferBuiltins.__set_file_attribute(inputStream);
    outputStream = new OutputStream();
  }

  public InputStream getInputStream() throws IOException {
    return inputStream;
  }

  public OutputStream getOutputStream() throws IOException {
    return outputStream;
  }

  public void close() {
    InferBuiltins.__set_mem_attribute(this);
    InferBuiltins.__set_mem_attribute(inputStream);
    InferBuiltins.__set_mem_attribute(outputStream);
  }
}
