/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package frontend.c;

import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;

import java.io.IOException;

import utils.DebuggableTemporaryFolder;
import utils.InferException;
import utils.ClangFrontendUtils;

public class AssertionTest {

  String conditionalOperatorBasePath = "infer/tests/codetoanalyze/c/frontend/assertions/";

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  void frontendTest(String fileRelative) throws InterruptedException, IOException, InferException {
    ClangFrontendUtils.createAndCompareCDotFiles(
        folder,
        conditionalOperatorBasePath + fileRelative);
  }

  @Ignore @Test
  public void whenCaptureRunOnAssertExampleThenDotFilesAreTheSame()
      throws InterruptedException, IOException, InferException {
    frontendTest("assert_example.c");
  }

}
