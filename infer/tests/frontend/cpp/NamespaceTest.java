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

public class NamespaceTest {
  String basePath = "infer/tests/codetoanalyze/cpp/frontend/namespace/";

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  void frontendTest(String fileRelative) throws InterruptedException, IOException, InferException {
    ClangFrontendUtils.createAndCompareCppDotFiles(folder, basePath + fileRelative);
  }

  @Test
  public void testNamespaceDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("namespace.cpp");
  }

  @Test
  public void testFunctionDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("function.cpp");
  }

  @Test
  public void testGlobalVariableDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("global_variable.cpp");
  }
}
