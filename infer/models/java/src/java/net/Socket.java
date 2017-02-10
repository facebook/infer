/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.net;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import com.facebook.infer.builtins.InferBuiltins;

public class Socket {

  InputStream inputStream;
  OutputStream outputStream;

  public Socket() {
    InferBuiltins.__set_file_attribute(this);
    inputStream = new InputStream();
    InferBuiltins.__set_file_attribute(inputStream);
    outputStream = new OutputStream();
    InferBuiltins.__set_file_attribute(outputStream);
  }

  public InputStream getInputStream() throws IOException {
    InferBuiltins.__check_untainted(this);
    return inputStream;
  }

  public OutputStream getOutputStream() throws IOException {
    InferBuiltins.__check_untainted(this);
    return outputStream;
  }

  public void close() {
    InferBuiltins.__set_mem_attribute(this);
    InferBuiltins.__set_mem_attribute(inputStream);
    InferBuiltins.__set_mem_attribute(outputStream);
  }

}
