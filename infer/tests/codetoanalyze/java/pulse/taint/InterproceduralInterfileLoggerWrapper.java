/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

class Logger {
  static void log(int s) {}
}

public class InterproceduralInterfileLoggerWrapper {
  static void log(int s) {
    Logger.log(s);
  }
}
