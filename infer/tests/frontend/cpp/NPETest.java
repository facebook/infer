/*
 * Copyright (c) 2016 - present Facebook, Inc.
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

import utils.ClangFrontendUtils;
import utils.DebuggableTemporaryFolder;
import utils.InferException;

public class NPETest {


  String methodBasePath = "infer/tests/codetoanalyze/cpp/errors/npe/";

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  void frontendTest(String fileRelative) throws InterruptedException, IOException, InferException {
    ClangFrontendUtils.createAndCompareCppDotFiles(folder, methodBasePath + fileRelative);
  }

  @Test
  public void testInlineMethodDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("method_call.cpp");
  }



}
