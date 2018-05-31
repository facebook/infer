/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import java.io.FileReader;
import java.io.DataInputStream;
import java.io.FileWriter;
import java.io.DataOutputStream;
import java.io.IOException;
import android.support.annotation.UiThread;

class JavaIO {
  FileReader reader;
  DataInputStream inputStream;
  FileWriter writer;
  DataOutputStream outputStream;

  int doFileRead() throws IOException {
    return reader.read();
  }

  String doStreamRead() throws IOException {
    return inputStream.readUTF();
  }

  @UiThread
  void fileReadBad() throws IOException {
    doFileRead();
  }

  @UiThread
  void streamReadBad() throws IOException {
    doStreamRead();
  }

  @UiThread
  void writerMethodsOk() throws IOException {
    writer = new FileWriter("bla");
    writer.write('a');
    writer.append('b');
    String enc = writer.getEncoding();
  }

  @UiThread
  void readerMethodsOk() throws IOException {
    reader = new FileReader("bla");
    String enc = reader.getEncoding();
    reader.markSupported();
    reader.reset();
    reader.close();
  }

  @UiThread
  void outputStreamMethodsOk() throws IOException {
    outputStream.write('a');
    outputStream.size();
  }

  @UiThread
  void inputStreamMethodsOk() throws IOException {
    inputStream.available();
    inputStream.reset();
    inputStream.close();
  }
}
