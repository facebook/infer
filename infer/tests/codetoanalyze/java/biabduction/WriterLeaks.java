/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PipedReader;
import java.io.PipedWriter;
import java.io.PrintWriter;
import java.io.Writer;

public class WriterLeaks {

  // Writer tests

  public void writerNotClosedAfterWrite() {
    Writer writer;
    try {
      writer = new PrintWriter("file.txt");
      writer.write(10);
      writer.close();
    } catch (IOException e) {
    }
  }

  public void writerClosed() throws IOException {
    Writer writer = null;
    try {
      writer = new PrintWriter("file.txt");
      writer.write(10);
    } catch (IOException e) {
    } finally {
      if (writer != null) writer.close();
    }
  }

  // PrintWriter tests

  public void printWriterNotClosedAfterAppend() {
    PrintWriter writer;
    try {
      writer = new PrintWriter("file.txt");
      writer = writer.append('0');
      writer.close();
    } catch (IOException e) {
    }
  }

  public void printWriterClosed() throws IOException {
    PrintWriter writer = null;
    try {
      writer = new PrintWriter("file.txt");
      writer = writer.append(null);
    } catch (IOException e) {
    } finally {
      if (writer != null) writer.close();
    }
  }

  // BufferedWriter tests

  public void bufferedWriterNotClosedAfterWrite() {
    BufferedWriter writer;
    try {
      FileWriter fw = new FileWriter("file.txt");
      writer = new BufferedWriter(fw);
      writer.write("word");
      writer.close();
    } catch (IOException e) {
    }
  }

  public void bufferedWriterClosed() throws IOException {
    BufferedWriter writer = null;
    try {
      FileWriter fw = new FileWriter("file.txt");
      writer = new BufferedWriter(fw);
      writer.flush();
    } catch (IOException e) {
    } finally {
      if (writer != null) writer.close();
    }
  }

  // OutputStreamWriter tests

  public void outputStreamWriterNotClosedAfterWrite() {
    OutputStreamWriter writer;
    try {
      writer = new OutputStreamWriter(new FileOutputStream("file.txt"));
      writer.write("word");
      writer.close();
    } catch (IOException e) {
    }
  }

  public void outputStreamWriterClosed() throws IOException {
    OutputStreamWriter writer = null;
    try {
      writer = new OutputStreamWriter(new FileOutputStream("file.txt"));
      writer.write(10);
    } catch (IOException e) {
    } finally {
      if (writer != null) writer.close();
    }
  }

  // FileWriter tests

  public void fileWriterNotClosedAfterWrite() {
    FileWriter writer;
    try {
      writer = new FileWriter("file.txt");
      writer.write("word");
      writer.close();
    } catch (IOException e) {
    }
  }

  public void fileWriterClosed() throws IOException {
    FileWriter writer = null;
    try {
      writer = new FileWriter("file.txt");
      writer.write(10);
    } catch (IOException e) {
    } finally {
      if (writer != null) writer.close();
    }
  }

  // PipedWriter tests

  public void pipedWriterNotClosedAfterConstructedWithReader() {
    PipedWriter writer;
    PipedReader reader;
    try {
      reader = new PipedReader();
      writer = new PipedWriter(reader);
      writer.write(42);
      writer.close();
    } catch (IOException e) {
    }
  }

  public void pipedWriterNotClosedAfterConnect(PipedReader reader) {
    PipedWriter writer;
    try {
      writer = new PipedWriter();
      writer.connect(reader);
      writer.write(42);
      writer.close();
    } catch (IOException e) {
    }
  }

  public void pipedWriterNotConnected() {
    PipedWriter writer;
    try {
      writer = new PipedWriter();
      writer.close();
    } catch (IOException e) {
    }
  }

  public void pipedWriterClosed(PipedReader reader) throws IOException {
    PipedWriter writer = null;
    try {
      writer = new PipedWriter();
      writer.connect(reader);
      writer.write(42);
    } catch (IOException e) {
    } finally {
      if (writer != null) writer.close();
    }
  }
}
