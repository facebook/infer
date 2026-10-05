/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;

// Streams returned by the java.nio.file.Files factories (issue #2110)
public class FilesLeaks {

  public void newInputStreamNotClosedAfterReadBad(Path path) throws IOException {
    InputStream stream = Files.newInputStream(path);
    stream.read();
  }

  public void newInputStreamClosedOk(Path path) throws IOException {
    InputStream stream = Files.newInputStream(path);
    try {
      stream.read();
    } finally {
      stream.close();
    }
  }

  public void newInputStreamTryWithResourcesOk(Path path) throws IOException {
    try (InputStream stream = Files.newInputStream(path)) {
      stream.read();
    }
  }

  public InputStream newInputStreamReturnedOk(Path path) throws IOException {
    return Files.newInputStream(path);
  }

  public void newOutputStreamNotClosedAfterWriteBad(Path path) throws IOException {
    OutputStream stream = Files.newOutputStream(path);
    stream.write(1);
  }

  public void newOutputStreamTryWithResourcesOk(Path path) throws IOException {
    try (OutputStream stream = Files.newOutputStream(path)) {
      stream.write(1);
    }
  }

  public void newBufferedReaderNotClosedAfterReadBad(Path path) throws IOException {
    BufferedReader reader = Files.newBufferedReader(path);
    reader.read();
  }

  public void newBufferedReaderTryWithResourcesOk(Path path) throws IOException {
    try (BufferedReader reader = Files.newBufferedReader(path)) {
      reader.readLine();
    }
  }

  public void newBufferedWriterNotClosedAfterWriteBad(Path path) throws IOException {
    BufferedWriter writer = Files.newBufferedWriter(path);
    writer.write(1);
  }

  public void newBufferedWriterTryWithResourcesOk(Path path) throws IOException {
    try (BufferedWriter writer = Files.newBufferedWriter(path)) {
      writer.write(1);
    }
  }
}
