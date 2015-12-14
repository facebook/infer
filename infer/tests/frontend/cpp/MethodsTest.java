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

public class MethodsTest {

  String methodBasePath = "infer/tests/codetoanalyze/cpp/frontend/methods/";

  @Rule
  public DebuggableTemporaryFolder folder = new DebuggableTemporaryFolder();

  void frontendTest(String fileRelative) throws InterruptedException, IOException, InferException {
    ClangFrontendUtils.createAndCompareCppDotFiles(folder, methodBasePath + fileRelative);
  }

  @Test
  public void testInlineMethodDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("inline_method.cpp");
  }

  @Test
  public void testDefaultParametersDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("default_parameters.cpp");
  }

  @Test
  public void testOverloadingDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("overloading.cpp");
  }

  @Test
  public void testDerefernceThisDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("dereference_this.cpp");
  }

  @Test
  public void testStaticDotFilesMatch()
      throws InterruptedException, IOException, InferException {
    frontendTest("static.cpp");
  }
}
