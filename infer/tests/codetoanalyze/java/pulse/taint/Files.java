/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import java.io.File;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.Paths;

public class Files {

  public File fileConstructorSinkBad() {
    String taintedString = (String) InferTaint.inferSecretSource();
    return new File(taintedString);
  }

  public Path fileSystemConstructorSinkBad1() {
    String taintedString = (String) InferTaint.inferSecretSource();
    return FileSystems.getDefault().getPath(taintedString);
  }

  // FN because varargs is stored in an array and taint doesn't look into arrays at the moment
  public Path FN_fileSystemConstructorSinkBad2() {
    String taintedString = (String) InferTaint.inferSecretSource();
    return FileSystems.getDefault().getPath("", taintedString);
  }

  public Path pathsSinkBad1() {
    String taintedString = (String) InferTaint.inferSecretSource();
    return Paths.get(taintedString);
  }

  // similar FN as above
  public Path FN_pathsSinkBad2() {
    String taintedString = (String) InferTaint.inferSecretSource();
    return Paths.get("", taintedString);
  }
}
