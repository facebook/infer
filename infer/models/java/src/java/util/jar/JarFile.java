/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.util.jar;

import java.io.File;
import java.io.IOException;
import java.util.zip.ZipFile;

public class JarFile extends ZipFile {

  public JarFile(String name) throws IOException {
    super(name);
  }

  public JarFile(String name, boolean verify) throws IOException {
    super(name);
  }

  public JarFile(File file) throws IOException {
    super("");
  }

  public JarFile(File file, boolean verify) throws IOException {
    this(file, verify, 0);
  }

  public JarFile(File file, boolean verify, int mode) throws IOException {
    super("");
  }

  public Manifest getManifest() throws IOException {
    throw new IOException();
  }

  public void close() throws IOException {
    super.close();
  }
}
