/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

import java.io.*;
import java.security.DigestInputStream;
import java.util.zip.CheckedInputStream;
import java.util.zip.DeflaterInputStream;
import java.util.zip.GZIPInputStream;
import java.util.zip.InflaterInputStream;
import javax.crypto.CipherInputStream;

public class FilterInputStreamLeaks {

  // BufferedInputStream  tests

  public void bufferedInputStreamNotClosedAfterReadBad() {
    FileInputStream fis;
    try {
      fis = new FileInputStream("file.txt");
      BufferedInputStream bis = new BufferedInputStream(fis);
      bis.read();
      bis.close();
    } catch (IOException e) {
    }
  }

  public void bufferedInputStreamClosedAfterResetOk() throws IOException {
    FileInputStream fis;
    BufferedInputStream bis = null;
    try {
      fis = new FileInputStream("file.txt");
      bis = new BufferedInputStream(fis);
      bis.reset();
    } catch (IOException e) {
    } finally {
      if (bis != null) bis.close();
    }
  }

  // CheckedInputStream  tests

  public void checkedInputStreamNotClosedAfterReadBad() {
    FileInputStream fis;
    try {
      fis = new FileInputStream("file.txt");
      CheckedInputStream chis = new CheckedInputStream(fis, null);
      chis.read();
      chis.close();
    } catch (IOException e) {
    }
  }

  public void checkedInputStreamClosedAfterSkipOk() throws IOException {
    FileInputStream fis;
    CheckedInputStream chis = null;
    try {
      fis = new FileInputStream("file.txt");
      chis = new CheckedInputStream(fis, null);
      chis.skip(5);
    } catch (IOException e) {
    } finally {
      if (chis != null) chis.close();
    }
  }

  // CipherInputStream  tests

  public void cipherInputStreamNotClosedAfterSkipBad() {
    FileInputStream fis;
    try {
      fis = new FileInputStream("file.txt");
      CipherInputStream cis = new CipherInputStream(fis, null);
      cis.skip(8);
      cis.close();
    } catch (IOException e) {
    }
  }

  public void cipherInputStreamClosedAfterReadOk() throws IOException {
    FileInputStream fis;
    CipherInputStream cis = null;
    try {
      fis = new FileInputStream("file.txt");
      cis = new CipherInputStream(fis, null);
      cis.read();
    } catch (IOException e) {
    } finally {
      if (cis != null) cis.close();
    }
  }

  // DataInputStream  tests

  public void dataInputStreamNotClosedAfterReadBad() {
    byte[] arr = new byte[10];
    FileInputStream fis;
    try {
      fis = new FileInputStream("file.txt");
      DataInputStream dis = new DataInputStream(fis);
      dis.read(arr);
      dis.close();
    } catch (IOException e) {
    }
  }

  public void dataInputStreamClosedAfterReadBooleanOk() throws IOException {
    FileInputStream fis;
    DataInputStream dis = null;
    try {
      fis = new FileInputStream("file.txt");
      dis = new DataInputStream(fis);
      dis.readBoolean();
    } catch (IOException e) {
    } finally {
      if (dis != null) dis.close();
    }
  }

  public static void try_with_resourceOK() throws IOException {
    try (DataInputStream dis = new DataInputStream(new FileInputStream("foo.txt"))) {
      int version = dis.readInt();
      if (version != 0) {
        throw new RuntimeException("Unexpected version " + version);
      }
    }
  }

  // DeflaterInputStream  tests

  public void deflaterInputStreamNotClosedAfterReadBad() {
    FileInputStream fis;
    try {
      fis = new FileInputStream("file.txt");
      DeflaterInputStream dis = new DeflaterInputStream(fis, null);
      dis.read();
      dis.close();
    } catch (IOException e) {
    }
  }

  public void deflaterInputStreamClosedAfterResetOk() throws IOException {
    FileInputStream fis;
    DeflaterInputStream dis = null;
    try {
      fis = new FileInputStream("file.txt");
      dis = new DeflaterInputStream(fis, null);
      dis.reset();
    } catch (IOException e) {
    } finally {
      if (dis != null) dis.close();
    }
  }

  // GZipInputStream  tests

  public void gzipInputStreamNotClosedAfterReadBad() {
    FileInputStream fis;
    try {
      fis = new FileInputStream("file.txt");
      GZIPInputStream gzipInputStream = new GZIPInputStream(fis);
      gzipInputStream.read();
      gzipInputStream.close();
    } catch (IOException e) {
    }
  }

  public void gzipInputStreamClosedAfterReadOk() throws IOException {
    FileInputStream fis = null;
    GZIPInputStream gzipInputStream = null;
    try {
      fis = new FileInputStream("file.txt");
      gzipInputStream = new GZIPInputStream(fis);
      gzipInputStream.read();
    } catch (IOException e) {
    } finally {
      if (gzipInputStream != null) gzipInputStream.close();
      else if (fis != null) fis.close();
    }
  }

  // DigestInputStream  tests

  public void digestInputStreamNotClosedAfterReadBad() {
    byte[] arr = new byte[10];
    FileInputStream fis;
    try {
      fis = new FileInputStream("file.txt");
      DigestInputStream dis = new DigestInputStream(fis, null);
      dis.read(arr);
      dis.close();
    } catch (IOException e) {
    }
  }

  public void digestInputStreamClosedAfterReadOk() throws IOException {
    FileInputStream fis;
    DigestInputStream dis = null;
    try {
      fis = new FileInputStream("file.txt");
      dis = new DigestInputStream(fis, null);
      dis.read();
    } catch (IOException e) {
    } finally {
      if (dis != null) dis.close();
    }
  }

  // InflaterInputStream  tests

  public void inflaterInputStreamNotClosedAfterReadBad() {
    FileInputStream fis;
    try {
      fis = new FileInputStream("file.txt");
      InflaterInputStream iis = new InflaterInputStream(fis, null);
      iis.read();
      iis.close();
    } catch (IOException e) {
    }
  }

  public void inflaterInputStreamClosedAfterAvailableOk() throws IOException {
    FileInputStream fis;
    InflaterInputStream iis = null;
    try {
      fis = new FileInputStream("file.txt");
      iis = new InflaterInputStream(fis, null);
      iis.available();
    } catch (IOException e) {
    } finally {
      if (iis != null) iis.close();
    }
  }

  // PushbackInputStream tests

  public void pushbackInputStreamNotClosedAfterReadBad() {
    FileInputStream fis;
    try {
      fis = new FileInputStream("file.txt");
      PushbackInputStream pms = new PushbackInputStream(fis);
      pms.read();
      pms.close();
    } catch (IOException e) {
    }
  }

  public void pushbackInputStreamClosedAfterResetOk() throws IOException {
    FileInputStream fis;
    PushbackInputStream pms = null;
    try {
      fis = new FileInputStream("file.txt");
      pms = new PushbackInputStream(fis);
      pms.reset();
    } catch (IOException e) {
    } finally {
      if (pms != null) pms.close();
    }
  }

  public void twoLevelWrapperNoLeakOk(File file) throws IOException {
    DataInputStream in = new DataInputStream(new BufferedInputStream(new FileInputStream(file)));
    in.close();
  }
}
