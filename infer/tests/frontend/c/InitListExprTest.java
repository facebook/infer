/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package frontend.c;

import org.junit.Rule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.ClangFrontendUtils;

public class InitListExprTest {

  String initListBasePath = "infer/tests/codetoanalyze/c/frontend/initialization/";

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  void frontendTest(String fileRelative) throws InterruptedException, IOException, InferException {
    ClangFrontendUtils.createAndCompareCDotFiles(folder, initListBasePath + fileRelative);
  }

  @Test
  public void whenCaptureRunOnArrayInitListExprThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("array_initlistexpr.c");
  }

  @Test
  public void whenCaptureRunOnStructInitListExprThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("struct_initlistexpr.c");
  }
}
