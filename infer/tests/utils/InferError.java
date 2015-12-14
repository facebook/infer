/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package utils;

import java.nio.file.Path;
import java.io.IOException;

public class InferError {

  private String errorType;
  private Path errorFile;
  private String errorMethod;
  private int errorLine;

  public InferError(String errorType, Path errorFile, String errorMethod, int errorLine) {
    this.errorType = errorType;
    this.errorMethod = errorMethod;
    this.errorLine = errorLine;
    try {
      this.errorFile = errorFile.toRealPath();
    } catch (IOException e) {
      this.errorFile = errorFile;
    }
  }

  public InferError(String errorType, Path errorFile, String errorMethod) {
    this.errorType = errorType;
    this.errorFile = errorFile;
    this.errorMethod = errorMethod;
    this.errorLine = -1;
  }

  public static InferError inferError(String errorMethod, Path errorFile, String errorType) {
    return new InferError(errorMethod, errorFile, errorType);
  }

  public String getErrorType() {
    return errorType;
  }

  public String getErrorMethod() {
    return errorMethod;
  }

  public Path getErrorFile() {
    return errorFile;
  }

  public int getErrorLine() {
    return errorLine;
  }

  public String toStringNoLine() {
    return errorType + " at " + errorFile + ", method " + errorMethod;
  }

  public String toString() {
    if (errorLine > 0)
      return toStringNoLine() + " (line " + errorLine + ")";
    else return toStringNoLine();
  }

  public String toStringFileMethod() {
    return errorFile + ", method " + errorMethod;
  }

  public String toStringErrorTypeMethod() {
    return errorType + " at " + errorMethod;
  }

  public boolean matchType(String type) {
    return this.errorType.equals(type);
  }

  public boolean matchFile(Path file) {
    Path realFile;
    try {
      realFile = file.toRealPath();
    } catch (IOException e) {
      realFile = file;
    }
    return this.errorFile.equals(realFile);
  }

  public boolean matchMethod(String method) {
    return this.errorMethod.equals(method);
  }

  public boolean matchLine(int line) {
    return this.errorLine < 0 || line < 0
        || (this.errorLine == line);
  }

}
