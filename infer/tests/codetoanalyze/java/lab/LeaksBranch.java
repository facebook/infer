/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

public class LeaksBranch {

  void mayLeakBad(Boolean b) throws IOException, FileNotFoundException {
    FileInputStream stream;
    if (b) {
      stream = new FileInputStream("file.txt");
    }
  }

  void choiceCloseOk(Boolean b) throws IOException, FileNotFoundException {
    FileInputStream stream = new FileInputStream("file.txt");
    if (b) {
      stream.close();
    } else {
      stream.close();
    }
  }
}
