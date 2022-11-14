/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import java.util.Scanner;

abstract class ScannerFail {
  void readOk() {
    Scanner scan = getScanner();
    while (scan.hasNext()) {
      scan.next();
    }
    scan.close();
  }

  void readBad() {
    Scanner scan = getScanner();
    scan.next();
    scan.close();
  }

  Scanner getScanner() {
    return new Scanner(System.in);
  }
}
