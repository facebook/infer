/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package utils.matchers;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import utils.InferError;

public class ErrorPattern {

  private final String errorType;

  private final Path errorFile;

  private final String errorMethod;

  public ErrorPattern(String type, String file, String method) {
    errorType = type;
    errorFile = Paths.get(file);
    errorMethod = method;
  }

  String getErrorType() {
    return errorType;
  }

  Path getErrorFile() {
    return errorFile;
  }

  String getErrorMethod() {
    return errorMethod;
  }

  public boolean match(InferError error) {
    return error.matchType(errorType)
        && error.matchMethod(errorMethod)
        && error.matchFile(errorFile);
  }

  public static List<ErrorPattern> createPatterns(String type, String file, String[] methods) {
    List<ErrorPattern> patterns = new ArrayList<>();
    for (String method : methods) {
      patterns.add(new ErrorPattern(type, file, method));
    }
    return patterns;
  }

  @Override
  public String toString() {
    return getErrorType() + " in file: " + getErrorFile() + ", method: " + getErrorMethod();
  }

}
