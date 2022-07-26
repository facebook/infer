/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PipedReader;
import java.io.PipedWriter;
import java.io.PushbackReader;
import java.io.Reader;

public class ReaderLeaks {

  private void ignore(Object o) {}

  // Reader  tests

  public void readerNotClosedAfterReadBad() {
    Reader r;
    try {
      r = new FileReader("testing.txt");
      r.read();
      r.close();
    } catch (IOException e) {
    }
  }

  public void readerClosedOk() throws IOException {
    Reader r = null;
    try {
      r = new FileReader("testing.txt");
      boolean ready = r.ready();
      r.close();
    } catch (IOException e) {
    } finally {
      if (r != null) r.close();
    }
  }

  // BufferedReader  tests

  public void bufferedReaderNotClosedAfterReadBad() {
    BufferedReader reader;
    try {
      reader = new BufferedReader(new FileReader("testing.txt"));
      ignore(reader.read());
      reader.close();
    } catch (IOException e) {
    }
  }

  public void bufferedReaderClosedOk() throws IOException {
    BufferedReader reader = null;
    try {
      reader = new BufferedReader(new FileReader("testing.txt"));
      ignore(reader.read());
    } catch (IOException e) {
    } finally {
      if (reader != null) reader.close();
    }
  }

  public void FN_tryWithBufferReaderWrapperBad(File file) throws IOException {
    try (InputStreamReader inputStreamReader = new InputStreamReader(new FileInputStream(file))) {
      BufferedReader reader = new BufferedReader(inputStreamReader);
      ignore(reader.readLine());
    }
  }

  public void tryWithBufferReaderWrapperOk(File file) throws IOException {
    try (InputStreamReader inputStreamReader = new InputStreamReader(new FileInputStream(file));
        BufferedReader reader = new BufferedReader(inputStreamReader); ) {
      ignore(reader.readLine());
    }
  }

  // InputStreamReader  tests

  public void inputStreamReaderNotClosedAfterReadBad() {
    InputStreamReader reader;
    try {
      reader = new InputStreamReader(new FileInputStream("testing.txt"));
      reader.read();
      reader.close();
    } catch (IOException e) {
    }
  }

  public void inputStreamReaderClosedOk() throws IOException {
    InputStreamReader reader = null;
    try {
      reader = new InputStreamReader(new FileInputStream("testing.txt"));
      ignore(reader.read());
    } catch (IOException e) {
    } finally {
      if (reader != null) reader.close();
    }
  }

  // FileReader  tests

  public void fileReaderNotClosedAfterReadBad() {
    FileReader reader;
    try {
      reader = new FileReader("testing.txt");
      reader.read();
      reader.close();
    } catch (IOException e) {
    }
  }

  public void fileReaderClosedOk() throws IOException {
    FileReader reader = null;
    try {
      reader = new FileReader("testing.txt");
      reader.read();
    } catch (IOException e) {
    } finally {
      if (reader != null) reader.close();
    }
  }

  // PushbackReader  tests

  public void pushbackReaderNotClosedAfterReadBad() {
    PushbackReader reader;
    try {
      reader = new PushbackReader(new InputStreamReader(new FileInputStream("testing.txt")));
      reader.read();
      reader.close();
    } catch (IOException e) {
    }
  }

  public void pushbackReaderClosedOk() throws IOException {
    PushbackReader reader = null;
    try {
      reader = new PushbackReader(new InputStreamReader(new FileInputStream("testing.txt")));
      reader.read();
    } catch (IOException e) {
    } finally {
      if (reader != null) reader.close();
    }
  }

  // PipedReader tests

  public void pipedReaderNotClosedAfterConstructedWithWriterBad() {
    PipedReader reader;
    PipedWriter writer;
    try {
      writer = new PipedWriter();
      reader = new PipedReader(writer);
      reader.read();
      reader.close();
    } catch (IOException e) {
    }
  }

  public void pipedReaderNotClosedAfterConnectOk(PipedWriter writer) {
    PipedReader reader;
    try {
      reader = new PipedReader();
      reader.connect(writer);
      reader.read();
      reader.close();
    } catch (IOException e) {
    }
  }

  public void FN_pipedReaderNotConnectedBad(PipedWriter writer) {
    PipedReader reader;
    try {
      reader = new PipedReader();
      reader.connect(writer);
      reader.close();
    } catch (IOException e) {
    }
  }

  public void pipedReaderClosedOk(PipedWriter writer) throws IOException {
    PipedReader reader = null;
    try {
      reader = new PipedReader();
      reader.connect(writer);
      reader.read();
    } catch (IOException e) {
    } finally {
      if (reader != null) reader.close();
    }
  }

  public void FN_pipedReaderWriterBad() throws IOException {
    PipedReader reader = null;
    PipedWriter writer = new PipedWriter();
    try {
      reader = new PipedReader(writer);
      reader.read();
    } catch (IOException e) {
    } finally {
      if (reader != null) reader.close();
    }
  }

  public void pipedReaderWriterOk() throws IOException {
    PipedReader reader = null;
    PipedWriter writer = new PipedWriter();
    try {
      reader = new PipedReader(writer);
      reader.read();
    } catch (IOException e) {
    } finally {
      if (reader != null) reader.close();
      else writer.close();
    }
  }
}
