/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import java.io.Closeable;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

public class LeaksAccessPathsInterprocedural {

  void closeResourceOk(Closeable c) throws IOException {
    c.close();
  }

  void closeResourceWrapperOk(Closeable c) throws IOException {
    closeResourceOk(c);
  }

  void closeResourceDirectOK() throws IOException, FileNotFoundException {
    closeResourceOk(new FileInputStream("file.txt"));
  }

  void closeResourceTransitiveOk() throws IOException, FileNotFoundException {
    closeResourceOk(new FileInputStream("file.txt"));
  }

  void closeOne(Closeable c1, Closeable c2) throws IOException {
    c2.close();
  }

  void closeOnlyOneBad() throws IOException, FileNotFoundException {
    closeOne(new FileInputStream("1.txt"), new FileInputStream("2.txt")); // warning
  }
}
