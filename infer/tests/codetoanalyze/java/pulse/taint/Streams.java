/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import java.io.IOException;
import java.io.InputStream;

public class Streams {

  int bufferSize = 1024;

  void copyBad() throws IOException {
    InputStream tainted = (InputStream) InferTaint.inferSecretSource();
    InferTaint.inferSensitiveSink(read(tainted.toString().getBytes()));
  }

  void copyBadFN() throws IOException {
    InputStream tainted = (InputStream) InferTaint.inferSecretSource();
    InferTaint.inferSensitiveSink(read(tainted));
  }

  void copyBad1FN() throws IOException {
    InputStream tainted = (InputStream) InferTaint.inferSecretSource();
    byte[] data = new byte[24];
    tainted.read(data);
    InferTaint.inferSensitiveSink(data);
  }

  void systemArrayCopyBadFN() throws IOException {
    InputStream tainted = (InputStream) InferTaint.inferSecretSource();
    byte[] data = read(tainted.toString().getBytes());
    byte[] buffer = new byte[bufferSize];
    System.arraycopy(data, 0, buffer, 0, data.length);
    InferTaint.inferSensitiveSink(buffer);
  }

  byte[] read(InputStream is) throws IOException {
    return read(is.toString().getBytes());
  }

  byte[] read(byte[] data) {
    byte[] buffer = new byte[bufferSize];
    for (int i = 0; i < data.length; ++i) {
      buffer[i] = data[i];
    }
    return buffer;
  }
}
