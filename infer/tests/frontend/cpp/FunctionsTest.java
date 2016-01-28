/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package frontend.cpp;

import org.junit.Rule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.ClangFrontendUtils;

public class FunctionsTest {

  String basePath = "infer/tests/codetoanalyze/cpp/frontend/types/";

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  void frontendTest(String fileRelative) throws InterruptedException, IOException, InferException {
    ClangFrontendUtils.createAndCompareCppDotFiles(folder, basePath + fileRelative);
  }

  @Test
  public void whenCaptureRunCommaThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("functions.cpp");
  }


  @Test
  public void testOperatorDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("operator_overload.cpp");
  }

  @Test
  public void testReturnStructDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("return_struct.cpp");
  }
}
