/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import android.annotation.SuppressLint;
import java.io.PrintStream;

public class PrintfArgsChecker {

  void argumentsMatch(PrintStream out) {
    out.printf("Hello %s", "world");
  }

  @SuppressLint("CHECKERS_PRINTF_ARGS")
  void suppressed(PrintStream out) {
    out.printf("Hello %d", "world");
  }

  @SuppressLint("checkers-printf-args")
  void normalizedSuppressed(PrintStream out) {
    out.printf("Hello %d", "world");
  }

  @SuppressLint("OTHER_CHECKER")
  void notSuppressed(PrintStream out) {
    out.printf("Hello %d", "world");
  }

  void stringInsteadOfInteger(PrintStream out) {
    out.printf("Hello %d", "world");
  }

  void wrongNumberOfArguments(PrintStream out) {
    out.printf("Hello %d, World %s", 10, "string", 1.5);
  }

  Integer field;

  void fieldAccess(PrintStream out) {
    out.printf("%d %s%n", field, field.toString());
  }

  void stringConcat(PrintStream out) {
    out.printf("%s" + "%s", "hello", "world");
  }

  void formatStringIsNotLiteral(PrintStream out) {
    String format = "%s %s";
    out.printf(format, "hello", "world");
  }
}

@SuppressLint("checkers-printf-args")
class SuppressedPrintfArgsChecker {

  void classSuppressed(PrintStream out) {
    out.printf("Hello %d", "world");
  }
}
